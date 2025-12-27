package com.app.backend.services.notification;

import com.app.backend.models.constant.LoaiThongBao;
import com.app.backend.responses.notification.NotificationResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

public interface IThongBaoService {

    Page<NotificationResponse> getMyNotifications(Long currentUserId, PageRequest pageRequest);

    long getUnreadCount(Long currentUserId);

    void markRead(Long currentUserId, Long thongBaoId) throws Exception;

    void markAllRead(Long currentUserId);

    // ==== helper cho các module khác gọi ====
    void createNotification(
            Long nguoiGuiId,
            Long nguoiNhanId,
            String loai,
            String noiDung,
            String metadataJson
    );

    void notifyFriendRequest(Long senderId, Long receiverId, Long ketBanId);

    void notifyFriendAccepted(Long accepterId, Long requesterId, Long ketBanId);

    void notifyFriendDeclined(Long declinerId, Long requesterId, Long ketBanId);

    /**
     * Gửi thông báo cho tất cả người dùng trong hệ thống (broadcast)
     */
    void broadcastNotification(Long nguoiGuiId, String loai, String noiDung, String metadataJson);
}
