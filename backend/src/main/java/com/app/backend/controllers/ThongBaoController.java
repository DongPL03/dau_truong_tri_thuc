package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.notification.NotificationResponse;
import com.app.backend.services.notification.IThongBaoService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("${api.prefix}/notifications")
@RequiredArgsConstructor
public class ThongBaoController {

    private final IThongBaoService thongBaoService;
    private final SecurityUtils securityUtils;

    @GetMapping("/my")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getMyNotifications(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long uid = securityUtils.getLoggedInUserId();
        PageRequest pageRequest = PageRequest.of(page, limit);

        Page<NotificationResponse> result =
                thongBaoService.getMyNotifications(uid, pageRequest);
        PageResponse<NotificationResponse> data = PageResponse.fromPage(result);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy danh sách thông báo thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/unread-count")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> getUnreadCount() {
        Long uid = securityUtils.getLoggedInUserId();
        long count = thongBaoService.getUnreadCount(uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy số lượng thông báo chưa đọc thành công")
                        .data(count)
                        .build()
        );
    }

    @PutMapping("/{id}/read")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> markRead(@PathVariable("id") Long id) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        thongBaoService.markRead(uid, id);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Đánh dấu đã đọc thành công")
                        .build()
        );
    }

    @PutMapping("/read-all")
    @PreAuthorize("hasAnyRole('ROLE_USER','ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> markAllRead() {
        Long uid = securityUtils.getLoggedInUserId();
        thongBaoService.markAllRead(uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Đã đánh dấu tất cả thông báo là đã đọc")
                        .build()
        );
    }
}
