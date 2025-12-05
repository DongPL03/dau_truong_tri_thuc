package com.app.backend.components;

import com.app.backend.responses.notification.NotificationResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class NotificationWsPublisher {

    private final SimpMessagingTemplate messagingTemplate;

    public void publishToUser(Long userId, NotificationResponse payload) {
        if (userId == null) return;
        String dest = "/topic/notifications/" + userId;
        messagingTemplate.convertAndSend(dest, payload);
    }
}
