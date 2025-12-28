package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BlockUserDTO;
import com.app.backend.dtos.FriendRequestDTO;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.banbe.BlockedUserResponse;
import com.app.backend.responses.banbe.FriendRequestItemResponse;
import com.app.backend.responses.banbe.FriendSuggestionResponse;
import com.app.backend.responses.banbe.FriendSummaryResponse;
import com.app.backend.services.banbe.IBanBeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/friends")
@RequiredArgsConstructor
public class BanBeController {

    private final IBanBeService banBeService;
    private final SecurityUtils securityUtils;

    @PostMapping("/request")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> sendFriendRequest(
            @Valid @RequestBody FriendRequestDTO dto
    ) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.sendFriendRequest(currentUserId, dto);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Gửi lời mời kết bạn thành công")
                        .data(null)
                        .build()
        );
    }

    @PostMapping("/requests/{id}/accept")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> acceptRequest(@PathVariable Long id) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.acceptRequest(currentUserId, id);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Chấp nhận lời mời kết bạn thành công")
                        .data(null)
                        .build()
        );
    }

    @PostMapping("/requests/{id}/decline")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> declineRequest(@PathVariable Long id) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.declineRequest(currentUserId, id);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Từ chối lời mời kết bạn thành công")
                        .data(null)
                        .build()
        );
    }

    @PostMapping("/requests/{id}/cancel")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> cancelRequest(@PathVariable Long id) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.cancelRequest(currentUserId, id);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Huỷ lời mời kết bạn thành công")
                        .data(null)
                        .build()
        );
    }

    @DeleteMapping("/{friendUserId}")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> unfriend(@PathVariable Long friendUserId) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.unfriend(currentUserId, friendUserId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Huỷ kết bạn thành công")
                        .data(null)
                        .build()
        );
    }

    @GetMapping("/requests/incoming")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getIncoming() {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<FriendRequestItemResponse> data = banBeService.getIncomingRequests(currentUserId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách lời mời đến thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/requests/outgoing")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getOutgoing() {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<FriendRequestItemResponse> data = banBeService.getOutgoingRequests(currentUserId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách lời mời đã gửi thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getFriends() {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<FriendSummaryResponse> data = banBeService.getFriends(currentUserId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách bạn bè thành công")
                        .data(data)
                        .build()
        );
    }

    // ============== BLOCK ==============

    @PostMapping("/block")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> blockUser(@Valid @RequestBody BlockUserDTO dto) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.blockUser(currentUserId, dto);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Đã chặn người dùng thành công")
                        .data(null)
                        .build()
        );
    }

    @DeleteMapping("/block/{userId}")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> unblockUser(@PathVariable Long userId) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();
        banBeService.unblockUser(currentUserId, userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Đã bỏ chặn người dùng thành công")
                        .data(null)
                        .build()
        );
    }

    @GetMapping("/blocked")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getBlockedUsers() {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<BlockedUserResponse> data = banBeService.getBlockedUsers(currentUserId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách người đã chặn thành công")
                        .data(data)
                        .build()
        );
    }

    // ============== SUGGESTIONS ==============

    @GetMapping("/suggestions")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getSuggestions(
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<FriendSuggestionResponse> data = banBeService.getFriendSuggestions(currentUserId, limit);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy gợi ý kết bạn thành công")
                        .data(data)
                        .build()
        );
    }

    // ============== SEARCH ==============

    @GetMapping("/search")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> searchUsers(
            @RequestParam String keyword,
            @RequestParam(defaultValue = "20") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        List<FriendSummaryResponse> data = banBeService.searchUsers(currentUserId, keyword, limit);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Tìm kiếm người dùng thành công")
                        .data(data)
                        .build()
        );
    }

    // ============== RELATIONSHIP STATUS ==============

    @GetMapping("/status/{userId}")
    @PreAuthorize("hasRole('ROLE_USER') or hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getRelationshipStatus(@PathVariable Long userId) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        String status = banBeService.getRelationshipStatus(currentUserId, userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy trạng thái quan hệ thành công")
                        .data(status)
                        .build()
        );
    }
}

