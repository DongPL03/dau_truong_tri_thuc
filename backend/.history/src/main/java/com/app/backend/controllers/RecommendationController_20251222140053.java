package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.recommendation.RecommendationResponse;
import com.app.backend.services.recommendation.IRecommendationService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("${api.prefix}/recommendations")
@RequiredArgsConstructor
public class RecommendationController {

    private final IRecommendationService recommendationService;
    private final SecurityUtils securityUtils;

    @GetMapping("/me")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyRecommendations() {
        Long userId = securityUtils.getLoggedInUserId();
        RecommendationResponse data = recommendationService.getRecommendationsForUser(userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy gợi ý học tập thành công")
                        .data(data)
                        .build()
        );
    }
}


