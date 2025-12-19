package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.achievement.AchievementResponse;
import com.app.backend.services.thanhtich.IThanhTichService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/achievements")
@RequiredArgsConstructor
public class ThanhTichController {

    private final IThanhTichService thanhTichService;
    private final SecurityUtils securityUtils;

    @GetMapping("/me")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyAchievements() throws Exception {
        Long userId = securityUtils.getLoggedInUserId();
        List<AchievementResponse> data = thanhTichService.getAchievementsOfUser(userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách thành tích thành công")
                        .data(data)
                        .build()
        );
    }
}

