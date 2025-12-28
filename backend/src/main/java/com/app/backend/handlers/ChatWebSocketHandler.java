package com.app.backend.handlers;

import com.app.backend.dtos.chat.GuiTinNhanDTO;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.IThanhVienPhongChatRepository;
import com.app.backend.responses.chat.TinNhanResponse;
import com.app.backend.services.chat.IPhongChatService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.stereotype.Controller;

import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Controller
@RequiredArgsConstructor
@Slf4j
public class ChatWebSocketHandler {

    private final SimpMessagingTemplate messagingTemplate;
    private final IPhongChatService phongChatService;
    private final IThanhVienPhongChatRepository thanhVienRepository;

    // Lưu trạng thái online của users: userId -> sessionId
    private static final Map<Long, String> onlineUsers = new ConcurrentHashMap<>();

    /**
     * Gửi tin nhắn qua WebSocket
     * Client gửi: /app/chat.send
     */
    @MessageMapping("/chat.send")
    public void sendMessage(@Payload GuiTinNhanDTO dto, Principal principal) {
        try {
            if (principal == null) {
                log.warn("Unauthorized message attempt");
                return;
            }

            NguoiDung user = (NguoiDung) ((org.springframework.security.authentication.UsernamePasswordAuthenticationToken) principal).getPrincipal();

            // Gửi tin nhắn qua service
            TinNhanResponse message = phongChatService.sendMessage(user.getId(), dto);

            // Broadcast đến tất cả thành viên trong phòng chat
            messagingTemplate.convertAndSend(
                    "/topic/chat/" + dto.getPhongChatId(),
                    WebSocketMessage.builder()
                            .type("NEW_MESSAGE")
                            .data(message)
                            .build()
            );

            // Gửi notification cho những người không online trong phòng chat
            List<Long> memberIds = thanhVienRepository.findUserIdsExcluding(dto.getPhongChatId(), user.getId());
            for (Long memberId : memberIds) {
                messagingTemplate.convertAndSendToUser(
                        memberId.toString(),
                        "/queue/notifications",
                        WebSocketMessage.builder()
                                .type("NEW_CHAT_MESSAGE")
                                .data(Map.of(
                                        "phongChatId", dto.getPhongChatId(),
                                        "nguoiGui", user.getHoTen(),
                                        "noiDung", truncate(dto.getNoiDung(), 50)
                                ))
                                .build()
                );
            }

        } catch (Exception e) {
            log.error("Error sending message: ", e);
        }
    }

    /**
     * Đánh dấu đã đọc qua WebSocket
     * Client gửi: /app/chat.read/{roomId}
     */
    @MessageMapping("/chat.read/{roomId}")
    public void markAsRead(@DestinationVariable Long roomId, Principal principal) {
        try {
            if (principal == null) return;

            NguoiDung user = (NguoiDung) ((org.springframework.security.authentication.UsernamePasswordAuthenticationToken) principal).getPrincipal();

            phongChatService.markAsRead(user.getId(), roomId);

            // Thông báo cho các thành viên khác
            messagingTemplate.convertAndSend(
                    "/topic/chat/" + roomId,
                    WebSocketMessage.builder()
                            .type("USER_READ")
                            .data(Map.of("userId", user.getId()))
                            .build()
            );

        } catch (Exception e) {
            log.error("Error marking as read: ", e);
        }
    }

    /**
     * User bắt đầu gõ
     * Client gửi: /app/chat.typing/{roomId}
     */
    @MessageMapping("/chat.typing/{roomId}")
    public void userTyping(@DestinationVariable Long roomId, Principal principal) {
        if (principal == null) return;

        NguoiDung user = (NguoiDung) ((org.springframework.security.authentication.UsernamePasswordAuthenticationToken) principal).getPrincipal();

        messagingTemplate.convertAndSend(
                "/topic/chat/" + roomId,
                WebSocketMessage.builder()
                        .type("USER_TYPING")
                        .data(Map.of(
                                "userId", user.getId(),
                                "userName", user.getHoTen()
                        ))
                        .build()
        );
    }

    /**
     * User ngừng gõ
     * Client gửi: /app/chat.stopTyping/{roomId}
     */
    @MessageMapping("/chat.stopTyping/{roomId}")
    public void userStopTyping(@DestinationVariable Long roomId, Principal principal) {
        if (principal == null) return;

        NguoiDung user = (NguoiDung) ((org.springframework.security.authentication.UsernamePasswordAuthenticationToken) principal).getPrincipal();

        messagingTemplate.convertAndSend(
                "/topic/chat/" + roomId,
                WebSocketMessage.builder()
                        .type("USER_STOP_TYPING")
                        .data(Map.of("userId", user.getId()))
                        .build()
        );
    }

    /**
     * User online
     */
    public void userOnline(Long userId, String sessionId) {
        onlineUsers.put(userId, sessionId);
        broadcastUserStatus(userId, true);
    }

    /**
     * User offline
     */
    public void userOffline(Long userId) {
        onlineUsers.remove(userId);
        broadcastUserStatus(userId, false);
    }

    /**
     * Kiểm tra user online
     */
    public boolean isUserOnline(Long userId) {
        return onlineUsers.containsKey(userId);
    }

    private void broadcastUserStatus(Long userId, boolean online) {
        messagingTemplate.convertAndSend(
                "/topic/user-status",
                WebSocketMessage.builder()
                        .type("USER_STATUS")
                        .data(Map.of(
                                "userId", userId,
                                "online", online
                        ))
                        .build()
        );
    }

    private String truncate(String text, int maxLength) {
        if (text == null) return "";
        if (text.length() <= maxLength) return text;
        return text.substring(0, maxLength) + "...";
    }

    // WebSocket message wrapper
    @lombok.Data
    @lombok.Builder
    @lombok.AllArgsConstructor
    @lombok.NoArgsConstructor
    public static class WebSocketMessage {
        private String type;
        private Object data;
    }
}
