package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.bangxephang.LeaderboardEntryResponse;
import com.app.backend.services.bangxephang.IBangXepHangService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("${api.prefix}/leaderboard")
@RequiredArgsConstructor
public class BangXepHangController {
    private final IBangXepHangService bangXepHangService;
    private final SecurityUtils securityUtils;

    @GetMapping("/global")
    public ResponseEntity<ResponseObject> getGlobalLeaderboard(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit,
            @RequestParam(defaultValue = "ALL") String time_range,
            @RequestParam(required = false) Long chu_de_id,
            @RequestParam(required = false) Long bo_cau_hoi_id,
            @RequestParam(defaultValue = "false") boolean friend_only
    ) {
//        Long currentUserId = securityUtils.getLoggedInUserId();
        PageRequest pageRequest = PageRequest.of(page, limit);
        Page<LeaderboardEntryResponse> result = bangXepHangService.getGlobalLeaderboard(
                pageRequest,
                time_range,
                chu_de_id,
                bo_cau_hoi_id
//                currentUserId,
//                friend_only
        );
        PageResponse<LeaderboardEntryResponse> data = PageResponse.fromPage(result);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy bảng xếp hạng toàn cầu thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }
}
