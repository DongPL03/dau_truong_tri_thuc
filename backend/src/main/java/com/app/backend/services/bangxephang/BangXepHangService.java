package com.app.backend.services.bangxephang;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.BangXepHang;
import com.app.backend.models.LichSuTranDau;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.constant.RankTier;
import com.app.backend.repositories.IBangXepHangRepository;
import com.app.backend.repositories.ILichSuTranDauRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.LeaderboardAggregateProjection;
import com.app.backend.responses.bangxephang.LeaderboardEntryResponse;
import com.app.backend.responses.bangxephang.WeeklyRankRewardResponse;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.user.UserSummaryResponse;
import com.app.backend.utils.LevelInfo;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.WeekFields;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BangXepHangService implements IBangXepHangService {

    private final IBangXepHangRepository bangXepHangRepository;
    private final ILichSuTranDauRepository lichSuTranDauRepository;
    private final INguoiDungRepository nguoiDungRepository;

    private static final long BASE_XP = 500L;
    private static final double GROWTH_RATE = 1.1;

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
            RankTier tier = calculateRankTier(tongDiem);

            return LeaderboardEntryResponse.builder()
                    .userId(user.getId())
                    .hoTen(user.getHoTen())
                    .anhDaiDien(user.getAvatarUrl())
                    .tongDiem(bxh.getTongDiem())
                    .tongTran(tongTran)
                    .soTranThang(soThang)
                    .soTranThua(soThua)
                    .tiLeThang(winRate)
//                    .xepHang(rank)
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
            RankTier tier = calculateRankTier(tongDiem);

            return LeaderboardEntryResponse.builder()
                    .userId(user.getId())
                    .hoTen(user.getHoTen())
                    .anhDaiDien(user.getAvatarUrl())
                    .tongDiem(tongDiem)
                    .tongTran(tongTran)
                    .soTranThang(soThang)
                    .soTranThua(soThua)
                    .tiLeThang(winRate)
//                    .xepHang(rank)
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
        RankTier tier = calculateRankTier(tongDiem);
        Pageable pageable = PageRequest.of(0, 10);

        // Gọi hàm có sẵn trong ILichSuTranDauRepository bạn đã viết
        Page<LichSuTranDau> historyPage = lichSuTranDauRepository.findByNguoiDung_IdOrderByHoanThanhLucDesc(userId, pageable);

        // Map từ Entity sang DTO
        List<LichSuTranDauResponse> listLichSu = historyPage.getContent().stream()
                .map(LichSuTranDauResponse::fromEntity)
                .collect(Collectors.toList());


        // 3️⃣ Build response
        return UserSummaryResponse.from(
                bxh != null ? bxh : BangXepHang.builder().build(),
                user,
                listLichSu,
                this
        );
    }

    @Override
    @Transactional
    public void recalcAllRankings() {
        bangXepHangRepository.updateAllRankings();
    }

    @Override
    @Transactional
    public WeeklyRankRewardResponse claimWeeklyReward(Long userId) throws Exception {
        BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của bạn"));

        String currentWeekId = getCurrentWeekId();
        String lastWeekId = bxh.getLastRankRewardWeek();

        long goldBefore = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;

        // đã nhận trong tuần này rồi
        if (currentWeekId.equals(lastWeekId)) {
            return WeeklyRankRewardResponse.builder()
                    .claimedBefore(true)
                    .goldReward(0L)
                    .rankTier(bxh.getRankTier())
                    .globalRank(bxh.getXepHang())
                    .weekId(currentWeekId)
                    .goldBefore(goldBefore)
                    .goldAfter(goldBefore)
                    .build();
        }

        // tính thưởng
        long reward = computeWeeklyGoldReward(bxh);
        long goldAfter = goldBefore + reward;

        bxh.setTienVang(goldAfter);
        bxh.setLastRankRewardWeek(currentWeekId);
        bangXepHangRepository.save(bxh);

        return WeeklyRankRewardResponse.builder()
                .claimedBefore(false)
                .goldReward(reward)
                .rankTier(bxh.getRankTier())
                .globalRank(bxh.getXepHang())
                .weekId(currentWeekId)
                .goldBefore(goldBefore)
                .goldAfter(goldAfter)
                .build();
    }


    private int safeToInt(Long value) {
        return value != null ? value.intValue() : 0;
    }


    public long xpNeededForNextLevel(int level) {
        if (level < 1) return BASE_XP;
        double xp = BASE_XP * Math.pow(GROWTH_RATE, level - 1);
        return (long) xp;
    }

    // Tính level hiện tại dựa trên tổng XP
    public LevelInfo computeLevelInfo(long totalXp) {
        if (totalXp < 0) totalXp = 0;
        int level = 1;
        long remaining = totalXp;

        while (true) {
            long needed = xpNeededForNextLevel(level);
            if (remaining < needed) break;
            remaining -= needed;
            level++;
            if (level > 1000) break;
        }

        long xpToNext = xpNeededForNextLevel(level) - remaining;
        double progressPercent = (double) remaining / xpNeededForNextLevel(level) * 100.0;

        return new LevelInfo(level, remaining, xpToNext, progressPercent);
    }

    public RankTier calculateRankTier(int totalPoints) {
        return RankTier.fromPoints(totalPoints);
    }

    /**
     * Lấy RankTier hiện tại của 1 bản ghi BXH dựa trên tongDiem.
     */
    public RankTier getRankTier(BangXepHang bxh) {
        int points = bxh.getTongDiem() != null ? bxh.getTongDiem() : 0;
        return RankTier.fromPoints(points);
    }


    // XP nhận từ 1 trận
    public long calculateXpFromMatch(int score, boolean win) {
        long baseXp = score / 10L;
        long matchBonus = win ? 150L : 40L;
        return Math.max(0, baseXp + matchBonus);
    }

    // Vàng nhận từ 1 trận
    public long calculateGoldFromMatch(int score, boolean win, boolean isRanked, RankTier rankTier) {
        long baseGold = 10L + (score / 200L);
        long rankedBonus = 0L;
        if (isRanked) {
            rankedBonus = win ? 25L : 10L;
        }
        double multi = (rankTier != null) ? rankTier.getMultiplier() : 1.0;
        long total = Math.round((baseGold + rankedBonus) * multi);
        return Math.max(0L, total);
    }


    private String getCurrentWeekId() {
        ZonedDateTime now = ZonedDateTime.now(ZoneId.of("Asia/Ho_Chi_Minh"));
        WeekFields wf = WeekFields.of(Locale.getDefault());
        int year = now.get(wf.weekBasedYear());
        int week = now.get(wf.weekOfWeekBasedYear());
        // ví dụ: 2025-05
        return String.format("%d-%02d", year, week);
    }

    private long computeWeeklyGoldReward(BangXepHang bxh) {
        RankTier tier = bxh.getRankTier() != null ? bxh.getRankTier() : RankTier.BRONZE;
        int globalRank = bxh.getXepHang() != null ? bxh.getXepHang() : Integer.MAX_VALUE;

        long base;
        switch (tier) {
            case SILVER -> base = 100;
            case GOLD -> base = 150;
            case PLATINUM -> base = 200;
            case DIAMOND -> base = 300;
            case MASTER -> base = 400;
            default -> base = 50;
        }

        double multiplier = 1.0;
        if (globalRank == 1) {
            multiplier = 2.0;
        } else if (globalRank <= 3) {
            multiplier = 1.5;
        } else if (globalRank <= 10) {
            multiplier = 1.3;
        } else if (globalRank <= 50) {
            multiplier = 1.1;
        }

        return Math.round(base * multiplier);
    }


}
