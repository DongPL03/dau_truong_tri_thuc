package com.app.backend.controllers;


import com.app.backend.models.constant.TrangThaiBoCauHoi;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.ITranDauRepository;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.admin.AdminDashboardStatsResponse;
import com.app.backend.responses.admin.BoCauHoiStatusStatsResponse;
import com.app.backend.responses.admin.UserNewPerDayResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.List;

@RestController
@RequestMapping("${api.prefix}/admin/dashboard")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
public class AdminDashboardController {

    private final INguoiDungRepository nguoiDungRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ITranDauRepository tranDauRepository;

    @GetMapping("/stats")
    public ResponseEntity<ResponseObject> getStats() {

        long tongNguoiDung = nguoiDungRepository.countByIsXoaFalse(); // hoặc countByIsXoaFalse()
        long tongBoCauHoi = boCauHoiRepository.count();
        long tongBoCauHoiChoDuyet = boCauHoiRepository.countByTrangThai(TrangThaiBoCauHoi.CHO_DUYET);
        long tongTranDau = tranDauRepository.count();

        AdminDashboardStatsResponse data = AdminDashboardStatsResponse.builder()
                .tong_nguoi_dung(tongNguoiDung)
                .tong_bo_cau_hoi(tongBoCauHoi)
                .tong_bo_cau_hoi_cho_duyet(tongBoCauHoiChoDuyet)
                .tong_tran_dau(tongTranDau)
                .build();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy thống kê dashboard admin thành công")
                        .data(data)
                        .build()
        );
    }


    @GetMapping("/user-new-per-day")
    public ResponseEntity<ResponseObject> getUserNewPerDay(
            @RequestParam(name = "days", defaultValue = "7") int days
    ) {
        if (days <= 0) {
            days = 7;
        }
        Instant fromTime = LocalDateTime.now()
                .minusDays(days)
                .atZone(java.time.ZoneId.systemDefault())
                .toInstant();

        List<Object[]> rawList = nguoiDungRepository.countNewUsersPerDayNative(fromTime);

        List<UserNewPerDayResponse> data = rawList.stream()
                .map(row -> {
                    Object dateObj = row[0];
                    Object countObj = row[1];

                    String ngayStr = String.valueOf(dateObj); // thường là yyyy-MM-dd
                    long soLuong = countObj == null ? 0L : ((Number) countObj).longValue();

                    return UserNewPerDayResponse.builder()
                            .ngay(ngayStr)
                            .so_luong(soLuong)
                            .build();
                })
                .toList();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy số user mới theo ngày thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/bo-cau-hoi-status-stats")
    public ResponseEntity<ResponseObject> getBoCauHoiStatusStats() {

        long daDuyet = boCauHoiRepository.countByTrangThai(TrangThaiBoCauHoi.DA_DUYET);
        long choDuyet = boCauHoiRepository.countByTrangThai(TrangThaiBoCauHoi.CHO_DUYET);
        long tuChoi = boCauHoiRepository.countByTrangThai(TrangThaiBoCauHoi.TU_CHOI);

        BoCauHoiStatusStatsResponse data = BoCauHoiStatusStatsResponse.builder()
                .so_luong_da_duyet(daDuyet)
                .so_luong_cho_duyet(choDuyet)
                .so_luong_tu_choi(tuChoi)
                .build();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy thống kê trạng thái bộ câu hỏi thành công")
                        .data(data)
                        .build()
        );
    }


}
