package com.app.backend.services.thongke;

import com.app.backend.models.NguoiDung;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.ICauHoiRepository;
import com.app.backend.repositories.IKhoaHocRepository;
import com.app.backend.repositories.ILichSuTranDauRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.ITranDauRepository;
import com.app.backend.responses.thongke.AdminSummaryStatsResponse;
import com.app.backend.responses.thongke.DateCountResponse;
import com.app.backend.responses.thongke.TopBoCauHoiStatsResponse;
import com.app.backend.responses.thongke.TopPlayerStatsResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ThongKeAdminService implements IThongKeAdminService {

    private final INguoiDungRepository nguoiDungRepository;
    private final ITranDauRepository tranDauRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final IKhoaHocRepository khoaHocRepository;
    private final ILichSuTranDauRepository lichSuTranDauRepository;

    @Override
    public AdminSummaryStatsResponse getSummary() {
        // ========== NGƯỜI DÙNG ==========
        long tongNguoiDung = nguoiDungRepository.count();
        long nguoiDungActive = nguoiDungRepository.countActive();
        long nguoiDungBlocked = nguoiDungRepository.countBlocked();
        long nguoiDungDeleted = nguoiDungRepository.countDeleted();
        long soAdmin = nguoiDungRepository.countAdmins();
        
        // User mới hôm nay
        Instant startOfToday = LocalDate.now()
                .atStartOfDay(ZoneId.systemDefault())
                .toInstant();
        long nguoiDungMoiHomNay = nguoiDungRepository.countRegisteredToday(startOfToday);

        // ========== TRẬN ĐẤU ==========
        long tongTranDau = tranDauRepository.count();
        long tranDangCho = tranDauRepository.countByTrangThai(TrangThaiTranDau.PENDING);
        long tranDangDienRa = tranDauRepository.countByTrangThai(TrangThaiTranDau.ONGOING);
        long tranDaKetThuc = tranDauRepository.countByTrangThai(TrangThaiTranDau.FINISHED);
        long tranDaHuy = tranDauRepository.countByTrangThai(TrangThaiTranDau.CANCELLED);
        
        // Trận hôm nay
        long tranHomNay = tranDauRepository.countCreatedToday(startOfToday);

        // ========== BỘ CÂU HỎI / CÂU HỎI ==========
        long tongBoCauHoi = boCauHoiRepository.count();
        long boCauHoiDaDuyet = boCauHoiRepository.countByTrangThai("DA_DUYET");
        long boCauHoiChoDuyet = boCauHoiRepository.countByTrangThai("CHO_DUYET");
        long boCauHoiTuChoi = boCauHoiRepository.countByTrangThai("TU_CHOI");
        long tongCauHoi = cauHoiRepository.count();

        // ========== KHÓA HỌC ==========
        long tongKhoaHoc = khoaHocRepository.count();
        long khoaHocPublished = khoaHocRepository.countByTrangThai("PUBLISHED");
        long khoaHocDraft = khoaHocRepository.countByTrangThai("DRAFT");

        return AdminSummaryStatsResponse.builder()
                // Người dùng
                .tongNguoiDung(tongNguoiDung)
                .nguoiDungActive(nguoiDungActive)
                .nguoiDungBlocked(nguoiDungBlocked)
                .nguoiDungDeleted(nguoiDungDeleted)
                .soAdmin(soAdmin)
                .nguoiDungMoiHomNay(nguoiDungMoiHomNay)
                // Trận đấu
                .tongTranDau(tongTranDau)
                .tranDangCho(tranDangCho)
                .tranDangDienRa(tranDangDienRa)
                .tranDaKetThuc(tranDaKetThuc)
                .tranDaHuy(tranDaHuy)
                .tranHomNay(tranHomNay)
                // Bộ câu hỏi
                .tongBoCauHoi(tongBoCauHoi)
                .boCauHoiDaDuyet(boCauHoiDaDuyet)
                .boCauHoiChoDuyet(boCauHoiChoDuyet)
                .boCauHoiTuChoi(boCauHoiTuChoi)
                .tongCauHoi(tongCauHoi)
                // Khóa học
                .tongKhoaHoc(tongKhoaHoc)
                .khoaHocPublished(khoaHocPublished)
                .khoaHocDraft(khoaHocDraft)
                .build();
    }

    @Override
    public List<DateCountResponse> getBattlesByDay(int days) {
        if (days <= 0) days = 7;
        Instant from = Instant.now().minus(days, ChronoUnit.DAYS);

        List<Object[]> raw = tranDauRepository.countBattlesByDaySince(from);

        return raw.stream().map(row -> {
            Object dateObj = row[0];
            LocalDate date;

            if (dateObj instanceof LocalDate localDate) {
                date = localDate;
            } else if (dateObj instanceof java.sql.Date sqlDate) {
                date = sqlDate.toLocalDate();
            } else if (dateObj instanceof java.util.Date utilDate) {
                date = utilDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
            } else {
                throw new IllegalStateException("Không parse được kiểu ngày cho thống kê trận");
            }

            long count = (row[1] instanceof Number num) ? num.longValue() : 0L;

            return DateCountResponse.builder()
                    .ngay(date)
                    .soLuong(count)
                    .build();
        }).toList();
    }

    @Override
    public List<TopBoCauHoiStatsResponse> getTopBoCauHoi(int limit) {
        if (limit <= 0) limit = 5;

        var page = tranDauRepository.findTopBoCauHoiByBattleCount(
                TrangThaiTranDau.FINISHED,
                PageRequest.of(0, limit)
        );

        return page.stream()
                .map(row -> {
                    Long boCauHoiId = (Long) row[0];
                    String tieuDe = (String) row[1];
                    long soTran = (row[2] instanceof Number num) ? num.longValue() : 0L;

                    return TopBoCauHoiStatsResponse.builder()
                            .boCauHoiId(boCauHoiId)
                            .tieuDe(tieuDe)
                            .soTran(soTran)
                            .build();
                })
                .toList();
    }

    @Override
    public List<TopPlayerStatsResponse> getTopPlayers(int limit) {
        if (limit <= 0) limit = 10;

        var pageAgg = lichSuTranDauRepository.aggregateLeaderboard(
                null, // from - không giới hạn thời gian
                null, // to
                null, // chuDeId
                null, // boCauHoiId
                PageRequest.of(0, limit)
        );

        return pageAgg.stream()
                .map(agg -> {
                    Long userId = agg.getUserId();
                    NguoiDung user = nguoiDungRepository.getReferenceById(userId);

                    long tongDiem = agg.getTongDiem() != null ? agg.getTongDiem().longValue() : 0L;
                    long tongTran = agg.getTongTran() != null ? agg.getTongTran().longValue() : 0L;
                    long soThang = agg.getSoTranThang() != null ? agg.getSoTranThang().longValue() : 0L;

                    double tiLeThang = tongTran > 0 ? (soThang * 100.0 / tongTran) : 0.0;

                    return TopPlayerStatsResponse.builder()
                            .userId(userId)
                            .hoTen(user.getHoTen())
                            .tenDangNhap(user.getTenDangNhap())
                            .avatarUrl(user.getAvatarUrl())
                            .tongDiem(tongDiem)
                            .tongTran(tongTran)
                            .soTranThang(soThang)
                            .tiLeThang(Math.round(tiLeThang * 100.0) / 100.0)
                            .build();
                })
                .toList();
    }
}
