// src/main/java/com/app/backend/components/ChatWsPublisher.java
package com.app.backend.components;

import com.app.backend.models.TinNhan;
import com.app.backend.responses.chat.ChatMessageResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ChatWsPublisher {

    private final SimpMessagingTemplate messagingTemplate;

    public void publishNewMessage(TinNhan tinNhan) {
        Long senderId = tinNhan.getGuiBoi().getId();
        Long receiverId = tinNhan.getNhanBoi().getId();

        // ChatMessageResponse.fromEntity(tinNhan, viewerId) bạn đã dùng ở REST
        ChatMessageResponse msgForSender = ChatMessageResponse.fromEntity(tinNhan, senderId);
        ChatMessageResponse msgForReceiver = ChatMessageResponse.fromEntity(tinNhan, receiverId);

        // Gửi cho người gửi
        messagingTemplate.convertAndSend(
                "/topic/chat/user/" + senderId,
                msgForSender
        );

        // Gửi cho người nhận
        messagingTemplate.convertAndSend(
                "/topic/chat/user/" + receiverId,
                msgForReceiver
        );
    }
}
