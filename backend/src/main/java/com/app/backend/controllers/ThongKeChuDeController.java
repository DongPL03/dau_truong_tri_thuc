package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.thongke.TopicStatResponse;
import com.app.backend.services.thongke.IThongKeChuDeService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/stats/topics")
@RequiredArgsConstructor
public class ThongKeChuDeController {

    private final IThongKeChuDeService thongKeChuDeService;
    private final SecurityUtils securityUtils;

    @GetMapping("/me")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyTopicStats() throws Exception {
        Long userId = securityUtils.getLoggedInUserId();
        List<TopicStatResponse> data = thongKeChuDeService.getTopicStatsForUser(userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thống kê theo chủ đề thành công")
                        .data(data)
                        .build()
        );
    }
}
