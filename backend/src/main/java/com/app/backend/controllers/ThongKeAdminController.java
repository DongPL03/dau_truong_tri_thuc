package com.app.backend.controllers;

import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.thongke.AdminSummaryStatsResponse;
import com.app.backend.responses.thongke.DateCountResponse;
import com.app.backend.responses.thongke.TopBoCauHoiStatsResponse;
import com.app.backend.responses.thongke.TopPlayerStatsResponse;
import com.app.backend.services.thongke.IThongKeAdminService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/admin/stats")
@RequiredArgsConstructor
public class ThongKeAdminController {

    private final IThongKeAdminService thongKeAdminService;

    /**
     * 🔹 Thống kê tổng quan hệ thống (Dùng cho card KPI trên Dashboard admin)
     */
    @GetMapping("/summary")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getSummary() {
        AdminSummaryStatsResponse data = thongKeAdminService.getSummary();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thống kê tổng quan hệ thống thành công")
                        .data(data)
                        .build()
        );
    }

    /**
     * 🔹 Số trận đấu theo ngày trong N ngày gần đây (dùng cho line chart)
     */
    @GetMapping("/battles-by-day")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getBattlesByDay(
            @RequestParam(defaultValue = "7") int days
    ) {
        List<DateCountResponse> data = thongKeAdminService.getBattlesByDay(days);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thống kê số trận theo ngày thành công")
                        .data(data)
                        .build()
        );
    }

    /**
     * 🔹 Top bộ câu hỏi được dùng nhiều nhất (dùng cho top list / chart)
     */
    @GetMapping("/top-bo-cau-hoi")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getTopBoCauHoi(
            @RequestParam(defaultValue = "5") int limit
    ) {
        List<TopBoCauHoiStatsResponse> data = thongKeAdminService.getTopBoCauHoi(limit);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy top bộ câu hỏi được sử dụng nhiều nhất thành công")
                        .data(data)
                        .build()
        );
    }

    /**
     * 🔹 Top người chơi (theo điểm tích lũy)
     */
    @GetMapping("/top-players")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getTopPlayers(
            @RequestParam(defaultValue = "10") int limit
    ) {
        List<TopPlayerStatsResponse> data = thongKeAdminService.getTopPlayers(limit);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy top người chơi thành công")
                        .data(data)
                        .build()
        );
    }
}
