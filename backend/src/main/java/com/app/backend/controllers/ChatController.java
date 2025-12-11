package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.SendMessageRequest;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.chat.ChatMessageResponse;
import com.app.backend.services.tinnhan.IChatService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("${api.prefix}/chat")
@RequiredArgsConstructor
public class ChatController {

    private final IChatService chatService;
    private final SecurityUtils securityUtils;

    /**
     * 📩 Gửi tin nhắn 1-1
     * POST /api/v1/chat/send
     */
    @PostMapping("/send")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> sendMessage(
            @Valid @RequestBody SendMessageRequest dto
    ) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();

        ChatMessageResponse data = chatService.sendMessage(currentUserId, dto);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Gửi tin nhắn thành công")
                        .data(data)
                        .build()
        );
    }

    /**
     * 💬 Lịch sử hội thoại 1-1
     * GET /api/v1/chat/conversation?friend_user_id=2&page=0&limit=20
     */
    @GetMapping("/conversation")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getConversation(
            @RequestParam("friend_user_id") Long friendUserId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit
    ) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();

        PageRequest pageRequest = PageRequest.of(page, limit);
        PageResponse<ChatMessageResponse> data =
                chatService.getConversation(currentUserId, friendUserId, pageRequest);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy lịch sử hội thoại thành công")
                        .data(data)
                        .build()
        );
    }

    /**
     * 📥 Inbox – tin nhắn gần đây (optional)
     * GET /api/v1/chat/inbox?page=0&limit=20
     */
    @GetMapping("/inbox")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getInbox(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int limit
    ) throws Exception {
        Long currentUserId = securityUtils.getLoggedInUserId();

        PageRequest pageRequest = PageRequest.of(page, limit);
        PageResponse<ChatMessageResponse> data =
                chatService.getMyInbox(currentUserId, pageRequest);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy inbox tin nhắn thành công")
                        .data(data)
                        .build()
        );
    }
}
