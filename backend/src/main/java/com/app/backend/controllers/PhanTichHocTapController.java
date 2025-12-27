package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.khoahoc.PhanTichHocTapResponse;
import com.app.backend.services.khoahoc.IPhanTichHocTapService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("${api.prefix}/phan-tich-hoc-tap")
@RequiredArgsConstructor
public class PhanTichHocTapController {

    private final IPhanTichHocTapService phanTichHocTapService;
    private final SecurityUtils securityUtils;

    @PostMapping("/khoa-hoc/{khoaHocId}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> phanTichKhoaHoc(@PathVariable Long khoaHocId) 
            throws DataNotFoundException {
        Long userId = securityUtils.getLoggedInUserId();
        PhanTichHocTapResponse data = phanTichHocTapService.phanTichKhoaHoc(userId, khoaHocId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Phân tích khóa học thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/khoa-hoc/{khoaHocId}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getPhanTich(@PathVariable Long khoaHocId) {
        Long userId = securityUtils.getLoggedInUserId();
        PhanTichHocTapResponse data = phanTichHocTapService.getPhanTich(userId, khoaHocId);
        if (data == null) {
            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .status(HttpStatus.NOT_FOUND)
                            .message("Chưa có phân tích cho khóa học này")
                            .data(null)
                            .build()
            );
        }
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy phân tích khóa học thành công")
                        .data(data)
                        .build()
        );
    }
}

