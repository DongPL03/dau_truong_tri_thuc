package com.app.backend.services.bangxephang;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.BangXepHang;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.IBangXepHangRepository;
import com.app.backend.repositories.ILichSuTranDauRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.LeaderboardAggregateProjection;
import com.app.backend.responses.bangxephang.LeaderboardEntryResponse;
import com.app.backend.responses.user.UserSummaryResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.atomic.AtomicInteger;

@Service
@RequiredArgsConstructor
public class BangXepHangService implements IBangXepHangService {

    private final IBangXepHangRepository bangXepHangRepository;
    private final ILichSuTranDauRepository lichSuTranDauRepository;
    private final INguoiDungRepository nguoiDungRepository;

    @Override
    public Page<LeaderboardEntryResponse> getGlobalLeaderboard(
            PageRequest pageRequest,
            String timeRange,
            Long chuDeId,
            Long boCauHoiId
//            Long currentUserId,
//            boolean friendOnly
    ) {

        boolean isAllMode = (timeRange == null || "ALL".equalsIgnoreCase(timeRange))
                && chuDeId == null
                && boCauHoiId == null;
//        && !friendOnly;

        if (isAllMode) {
            // ❖ mode ALL (mặc định): lấy trực tiếp từ bảng bang_xep_hang
            return mapFromBangXepHang(pageRequest);
        } else {
            // ❖ mode filter nâng cao: tính lại từ bảng lich_su_tran_dau
//            return mapFromLichSuTranDau(pageRequest, timeRange, chuDeId, boCauHoiId, currentUserId, friendOnly);
            return mapFromLichSuTranDau(pageRequest, timeRange, chuDeId, boCauHoiId);
        }
    }

    // ============================================================
    // 1. Mode ALL → dùng bảng bang_xep_hang
    // ============================================================
    private Page<LeaderboardEntryResponse> mapFromBangXepHang(PageRequest pageRequest) {
        Page<BangXepHang> page = bangXepHangRepository
                .findAllByOrderByTongDiemDescCapNhatLucAsc(pageRequest);

        AtomicInteger rankCounter =
                new AtomicInteger(pageRequest.getPageNumber() * pageRequest.getPageSize() + 1);

        return page.map(bxh -> {
            NguoiDung user = bxh.getNguoiDung();
            int tongTran = bxh.getTongTran() != null ? bxh.getTongTran() : 0;
            int soThang = bxh.getSoTranThang() != null ? bxh.getSoTranThang() : 0;
            int soThua = bxh.getSoTranThua() != null ? bxh.getSoTranThua() : 0;

            double winRate = 0.0;
            if (tongTran > 0) {
                winRate = soThang * 100.0 / tongTran;
            }

            int rank = rankCounter.getAndIncrement();
            int tongDiem = bxh.getTongDiem() != null ? bxh.getTongDiem() : 0;
            String tier = calcTierName(tongDiem);

            return LeaderboardEntryResponse.builder()
                    .userId(user.getId())
                    .hoTen(user.getHoTen())
                    .anhDaiDien(user.getAvatarUrl())
                    .tongDiem(bxh.getTongDiem())
                    .tongTran(tongTran)
                    .soTranThang(soThang)
                    .soTranThua(soThua)
                    .tiLeThang(winRate)
                    .xepHang(rank)
                    .rankTier(tier)
                    .build();
        });
    }

    // ============================================================
    // 2. Mode filter nâng cao → tính từ lich_su_tran_dau
    // ============================================================
    private Page<LeaderboardEntryResponse> mapFromLichSuTranDau(
            PageRequest pageRequest,
            String timeRange,
            Long chuDeId,
            Long boCauHoiId
//            Long currentUserId,
//            boolean friendOnly
    ) {

        Instant now = Instant.now();
        Instant from = null;
        Instant to = null;

        if ("WEEK".equalsIgnoreCase(timeRange)) {
            from = now.minus(Duration.ofDays(7));
            to = now;
        } else if ("MONTH".equalsIgnoreCase(timeRange)) {
            from = now.minus(Duration.ofDays(30));
            to = now;
        } else {
            // ALL hoặc null → không giới hạn thời gian
            from = null;
            to = null;
        }

        // Hiện tại friendOnly chưa có bảng bạn bè → tạm thời bỏ qua
        // Sau này có FriendRepository thì filter ở tầng service sau.

        Page<LeaderboardAggregateProjection> pageAgg =
                lichSuTranDauRepository.aggregateLeaderboard(
                        from,
                        to,
                        chuDeId,
                        boCauHoiId,
                        pageRequest
                );

        AtomicInteger rankCounter =
                new AtomicInteger(pageRequest.getPageNumber() * pageRequest.getPageSize() + 1);

        return pageAgg.map(agg -> {
            Long userId = agg.getUserId();
            NguoiDung user = nguoiDungRepository.getReferenceById(userId);

            int tongDiem = safeToInt(agg.getTongDiem());
            int tongTran = safeToInt(agg.getTongTran());
            int soThang = safeToInt(agg.getSoTranThang());
            int soThua = Math.max(0, tongTran - soThang);

            double winRate = 0.0;
            if (tongTran > 0) {
                winRate = soThang * 100.0 / tongTran;
            }

            int rank = rankCounter.getAndIncrement();
            String tier = calcTierName(tongDiem);

            return LeaderboardEntryResponse.builder()
                    .userId(user.getId())
                    .hoTen(user.getHoTen())
                    .anhDaiDien(user.getAvatarUrl())
                    .tongDiem(tongDiem)
                    .tongTran(tongTran)
                    .soTranThang(soThang)
                    .soTranThua(soThua)
                    .tiLeThang(winRate)
                    .xepHang(rank)
                    .rankTier(tier)
                    .build();
        });
    }

    @Override
    public UserSummaryResponse getUserSummary(Long userId) throws DataNotFoundException {
        // 1️⃣ Lấy thông tin user
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // 2️⃣ Lấy record BXH nếu có
        BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                .orElse(null);

        int tongDiem = 0;
        int tongTran = 0;
        int soThang = 0;
        int soThua = 0;

        if (bxh != null) {
            tongDiem = bxh.getTongDiem() != null ? bxh.getTongDiem() : 0;
            tongTran = bxh.getTongTran() != null ? bxh.getTongTran() : 0;
            soThang = bxh.getSoTranThang() != null ? bxh.getSoTranThang() : 0;
            soThua = bxh.getSoTranThua() != null ? bxh.getSoTranThua() : 0;
        }

        double tiLeThang = 0.0;
        if (tongTran > 0) {
            tiLeThang = soThang * 100.0 / tongTran;
        }
        String tier = calcTierName(tongDiem);


        // 3️⃣ Build response
        return UserSummaryResponse.builder()
                .userId(user.getId())
                .hoTen(user.getHoTen())
                .avatarUrl(user.getAvatarUrl())
                .tongDiem(tongDiem)
                .tongTran(tongTran)
                .soTranThang(soThang)
                .soTranThua(soThua)
                .tiLeThang(tiLeThang)
                .rankTier(tier)
                .build();
    }


    private int safeToInt(Long value) {
        return value != null ? value.intValue() : 0;
    }

    private String calcTierName(int totalScore) {
        if (totalScore >= 5000) {
            return "MASTER";
        } else if (totalScore >= 4000) {
            return "DIAMOND";
        } else if (totalScore >= 3000) {
            return "PLATINUM";
        } else if (totalScore >= 2000) {
            return "GOLD";
        } else if (totalScore >= 1000) {
            return "SILVER";
        } else {
            return "BRONZE";
        }
    }

}
