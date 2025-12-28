package com.app.backend.controllers;

import com.app.backend.dtos.chat.CapNhatPhongChatDTO;
import com.app.backend.dtos.chat.GuiTinNhanDTO;
import com.app.backend.dtos.chat.TaoPhongChatDTO;
import com.app.backend.models.NguoiDung;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.chat.PhongChatResponse;
import com.app.backend.responses.chat.TinNhanResponse;
import com.app.backend.services.chat.IPhongChatService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/chat")
@RequiredArgsConstructor
public class PhongChatController {

    private final IPhongChatService phongChatService;

    // ============== PHÒNG CHAT APIs ==============

    /**
     * Tạo phòng chat mới
     */
    @PostMapping("/rooms")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> createPhongChat(
            @AuthenticationPrincipal NguoiDung user,
            @Valid @RequestBody TaoPhongChatDTO dto) {
        try {
            PhongChatResponse response = phongChatService.createPhongChat(user.getId(), dto);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Tạo phòng chat thành công")
                            .data(response)
                            .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Tạo hoặc lấy phòng chat 1-1
     */
    @PostMapping("/rooms/private/{userId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getOrCreatePrivateChat(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long userId) {
        try {
            PhongChatResponse response = phongChatService.getOrCreatePrivateChat(user.getId(), userId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy phòng chat thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Lấy danh sách phòng chat
     */
    @GetMapping("/rooms")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getPhongChats(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<PhongChatResponse> rooms = phongChatService.getPhongChats(user.getId(), pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy danh sách phòng chat thành công")
                .data(PageResponse.fromPage(rooms))
                .build());
    }

    /**
     * Lấy danh sách phòng chat được ghim
     */
    @GetMapping("/rooms/pinned")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getPinnedPhongChats(
            @AuthenticationPrincipal NguoiDung user) {

        List<PhongChatResponse> rooms = phongChatService.getPinnedPhongChats(user.getId());

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy phòng chat được ghim thành công")
                .data(rooms)
                .build());
    }

    /**
     * Lấy chi tiết phòng chat
     */
    @GetMapping("/rooms/{roomId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getPhongChatDetail(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            PhongChatResponse response = phongChatService.getPhongChatDetail(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy chi tiết phòng chat thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Cập nhật phòng chat
     */
    @PutMapping("/rooms/{roomId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> updatePhongChat(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId,
            @Valid @RequestBody CapNhatPhongChatDTO dto) {
        try {
            PhongChatResponse response = phongChatService.updatePhongChat(user.getId(), roomId, dto);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Cập nhật phòng chat thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Rời phòng chat
     */
    @PostMapping("/rooms/{roomId}/leave")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> leavePhongChat(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            phongChatService.leavePhongChat(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Rời phòng chat thành công")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Xóa phòng chat
     */
    @DeleteMapping("/rooms/{roomId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> deletePhongChat(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            phongChatService.deletePhongChat(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Xóa phòng chat thành công")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Ghim/bỏ ghim phòng chat
     */
    @PostMapping("/rooms/{roomId}/pin")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> togglePin(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            PhongChatResponse response = phongChatService.togglePin(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(response.getDaGhim() ? "Đã ghim phòng chat" : "Đã bỏ ghim phòng chat")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Tắt/bật thông báo
     */
    @PostMapping("/rooms/{roomId}/mute")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> toggleMute(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            PhongChatResponse response = phongChatService.toggleMute(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(response.getDaTatThongBao() ? "Đã tắt thông báo" : "Đã bật thông báo")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Tìm kiếm phòng chat
     */
    @GetMapping("/rooms/search")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> searchPhongChats(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<PhongChatResponse> rooms = phongChatService.searchPhongChats(user.getId(), keyword, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Tìm kiếm phòng chat thành công")
                .data(PageResponse.fromPage(rooms))
                .build());
    }

    /**
     * Đếm tổng tin nhắn chưa đọc
     */
    @GetMapping("/unread-count")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> countTotalUnread(
            @AuthenticationPrincipal NguoiDung user) {

        long count = phongChatService.countTotalUnread(user.getId());

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy số tin nhắn chưa đọc thành công")
                .data(count)
                .build());
    }

    // ============== TIN NHẮN APIs ==============

    /**
     * Gửi tin nhắn
     */
    @PostMapping("/messages")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> sendMessage(
            @AuthenticationPrincipal NguoiDung user,
            @Valid @RequestBody GuiTinNhanDTO dto) {
        try {
            TinNhanResponse response = phongChatService.sendMessage(user.getId(), dto);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Gửi tin nhắn thành công")
                            .data(response)
                            .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Lấy tin nhắn trong phòng chat
     */
    @GetMapping("/rooms/{roomId}/messages")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getMessages(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "30") int limit) {
        try {
            Pageable pageable = PageRequest.of(page, limit);
            Page<TinNhanResponse> messages = phongChatService.getMessages(user.getId(), roomId, pageable);

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy tin nhắn thành công")
                    .data(PageResponse.fromPage(messages))
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Lấy tin nhắn cũ hơn (load more)
     */
    @GetMapping("/rooms/{roomId}/messages/before/{messageId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getMessagesBefore(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId,
            @PathVariable Long messageId,
            @RequestParam(defaultValue = "30") int limit) {
        try {
            Pageable pageable = PageRequest.of(0, limit);
            Page<TinNhanResponse> messages = phongChatService.getMessagesBefore(user.getId(), roomId, messageId, pageable);

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy tin nhắn thành công")
                    .data(PageResponse.fromPage(messages))
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Tìm kiếm tin nhắn
     */
    @GetMapping("/rooms/{roomId}/messages/search")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> searchMessages(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId,
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit) {
        try {
            Pageable pageable = PageRequest.of(page, limit);
            Page<TinNhanResponse> messages = phongChatService.searchMessages(user.getId(), roomId, keyword, pageable);

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Tìm kiếm tin nhắn thành công")
                    .data(PageResponse.fromPage(messages))
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Sửa tin nhắn
     */
    @PutMapping("/messages/{messageId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> editMessage(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long messageId,
            @RequestBody String noiDung) {
        try {
            TinNhanResponse response = phongChatService.editMessage(user.getId(), messageId, noiDung);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Sửa tin nhắn thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Xóa tin nhắn
     */
    @DeleteMapping("/messages/{messageId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> deleteMessage(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long messageId) {
        try {
            phongChatService.deleteMessage(user.getId(), messageId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Xóa tin nhắn thành công")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Ghim/bỏ ghim tin nhắn
     */
    @PostMapping("/messages/{messageId}/pin")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> togglePinMessage(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long messageId) {
        try {
            TinNhanResponse response = phongChatService.togglePinMessage(user.getId(), messageId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(response.getDaGhim() ? "Đã ghim tin nhắn" : "Đã bỏ ghim tin nhắn")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Lấy tin nhắn được ghim
     */
    @GetMapping("/rooms/{roomId}/messages/pinned")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getPinnedMessages(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            List<TinNhanResponse> messages = phongChatService.getPinnedMessages(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy tin nhắn được ghim thành công")
                    .data(messages)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Đánh dấu đã đọc
     */
    @PostMapping("/rooms/{roomId}/read")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> markAsRead(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long roomId) {
        try {
            phongChatService.markAsRead(user.getId(), roomId);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Đánh dấu đã đọc thành công")
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }
}
