package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.bangxephang.LeaderboardEntryResponse;
import com.app.backend.responses.bangxephang.WeeklyRankRewardResponse;
import com.app.backend.responses.user.UserSummaryResponse;
import com.app.backend.services.bangxephang.IBangXepHangService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

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

    /**
     * 🔹 Lấy thông tin tổng quan của 1 user trên BXH
     * Dùng cho:
     * - Trang BXH (click vào 1 dòng -> show modal)
     * - Trang chi tiết user (Admin) muốn xem nhanh thành tích
     */
    @GetMapping("/user/{userId}/summary")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getUserSummary(@PathVariable Long userId) throws Exception {
        UserSummaryResponse data = bangXepHangService.getUserSummary(userId);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy thông tin tổng quan người dùng trên bảng xếp hạng thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    /**
     * 🔹 ADMIN: Force tính lại thứ hạng (xếp_hạng) cho toàn bộ bảng xếp hạng.
     * Không đụng vào tổng điểm, chỉ update trường xep_hang theo tổng điểm hiện tại.
     */
    @PostMapping("/admin/recalc-rank")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> recalcRankings() {
        bangXepHangService.recalcAllRankings();

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Đã tính lại thứ hạng bảng xếp hạng thành công")
                        .status(HttpStatus.OK)
                        .data(null)
                        .build()
        );
    }

    /**
     * 🔹 USER: Nhận thưởng xếp hạng tuần
     */
    @PostMapping("/claim-weekly-reward")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> claimWeeklyReward() throws Exception {
        Long userId = securityUtils.getLoggedInUserId();

        WeeklyRankRewardResponse data = bangXepHangService.claimWeeklyReward(userId);

        String message;
        if (data.isClaimedBefore()) {
            message = "Bạn đã nhận thưởng xếp hạng cho tuần " + data.getWeekId() + " rồi";
        } else {
            message = "Nhận thưởng xếp hạng tuần thành công";
        }

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message(message)
                        .data(data)
                        .build()
        );
    }

}
