package com.app.backend.services.tinnhan;

import com.app.backend.dtos.SendMessageRequest;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.chat.ChatMessageResponse;
import org.springframework.data.domain.PageRequest;

public interface IChatService {

    /**
     * Gửi tin nhắn 1-1 (chỉ cho bạn bè)
     */
    ChatMessageResponse sendMessage(Long currentUserId, SendMessageRequest dto) throws Exception;

    /**
     * Lịch sử hội thoại 1-1 giữa currentUserId và friendUserId
     */
    PageResponse<ChatMessageResponse> getConversation(
            Long currentUserId,
            Long friendUserId,
            PageRequest pageRequest
    ) throws Exception;

    /**
     * (Optional) Inbox – danh sách tin nhắn gần đây (trộn tất cả cuộc trò chuyện)
     * Nếu chưa cần, bạn có thể bỏ không dùng ở Controller/UI.
     */
    PageResponse<ChatMessageResponse> getMyInbox(
            Long currentUserId,
            PageRequest pageRequest
    );
}


