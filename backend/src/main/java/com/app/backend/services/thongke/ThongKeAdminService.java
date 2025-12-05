package com.app.backend.services.thongke;

import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.ICauHoiRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.ITranDauRepository;
import com.app.backend.responses.thongke.AdminSummaryStatsResponse;
import com.app.backend.responses.thongke.DateCountResponse;
import com.app.backend.responses.thongke.TopBoCauHoiStatsResponse;
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

    @Override
    public AdminSummaryStatsResponse getSummary() {
        long tongNguoiDung = nguoiDungRepository.count();

        long tongTranDau = tranDauRepository.count();
        long tranDangCho = tranDauRepository.countByTrangThai(TrangThaiTranDau.PENDING);
        long tranDangDienRa = tranDauRepository.countByTrangThai(TrangThaiTranDau.ONGOING);
        long tranDaKetThuc = tranDauRepository.countByTrangThai(TrangThaiTranDau.FINISHED);

        long tongBoCauHoi = boCauHoiRepository.count();
        long tongCauHoi = cauHoiRepository.count();

        return AdminSummaryStatsResponse.builder()
                .tongNguoiDung(tongNguoiDung)
                .tongTranDau(tongTranDau)
                .tranDangCho(tranDangCho)
                .tranDangDienRa(tranDangDienRa)
                .tranDaKetThuc(tranDaKetThuc)
                .tongBoCauHoi(tongBoCauHoi)
                .tongCauHoi(tongCauHoi)
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
}
