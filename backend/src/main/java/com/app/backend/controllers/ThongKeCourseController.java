package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.thongke.CourseStatResponse;
import com.app.backend.services.thongke.ICourseStatService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("${api.prefix}/stats/courses")
@RequiredArgsConstructor
public class ThongKeCourseController {

    private final ICourseStatService courseStatService;
    private final SecurityUtils securityUtils;

    @GetMapping("/me")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyCourseStats() throws Exception {
        Long userId = securityUtils.getLoggedInUserId();
        CourseStatResponse data = courseStatService.getCourseStatsForUser(userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thống kê khóa học thành công")
                        .data(data)
                        .build()
        );
    }
}

