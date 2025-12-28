package com.app.backend.services.chat;

import com.app.backend.dtos.chat.CapNhatPhongChatDTO;
import com.app.backend.dtos.chat.GuiTinNhanDTO;
import com.app.backend.dtos.chat.TaoPhongChatDTO;
import com.app.backend.responses.chat.PhongChatResponse;
import com.app.backend.responses.chat.TinNhanResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface IPhongChatService {

    // ============== PHÒNG CHAT ==============

    /**
     * Tạo phòng chat mới (1-1 hoặc group)
     */
    PhongChatResponse createPhongChat(Long userId, TaoPhongChatDTO dto) throws Exception;

    /**
     * Tạo hoặc lấy phòng chat 1-1 với người dùng khác
     */
    PhongChatResponse getOrCreatePrivateChat(Long userId, Long otherUserId) throws Exception;

    /**
     * Lấy danh sách phòng chat của người dùng
     */
    Page<PhongChatResponse> getPhongChats(Long userId, Pageable pageable);

    /**
     * Lấy danh sách phòng chat được ghim
     */
    List<PhongChatResponse> getPinnedPhongChats(Long userId);

    /**
     * Lấy chi tiết phòng chat
     */
    PhongChatResponse getPhongChatDetail(Long userId, Long phongChatId) throws Exception;

    /**
     * Cập nhật phòng chat (tên, ảnh, thêm/xóa thành viên)
     */
    PhongChatResponse updatePhongChat(Long userId, Long phongChatId, CapNhatPhongChatDTO dto) throws Exception;

    /**
     * Rời khỏi phòng chat
     */
    void leavePhongChat(Long userId, Long phongChatId) throws Exception;

    /**
     * Xóa phòng chat (chỉ admin)
     */
    void deletePhongChat(Long userId, Long phongChatId) throws Exception;

    /**
     * Ghim/bỏ ghim phòng chat
     */
    PhongChatResponse togglePin(Long userId, Long phongChatId) throws Exception;

    /**
     * Tắt/bật thông báo
     */
    PhongChatResponse toggleMute(Long userId, Long phongChatId) throws Exception;

    /**
     * Tìm kiếm phòng chat
     */
    Page<PhongChatResponse> searchPhongChats(Long userId, String keyword, Pageable pageable);

    /**
     * Đếm tổng số tin nhắn chưa đọc
     */
    long countTotalUnread(Long userId);

    // ============== TIN NHẮN ==============

    /**
     * Gửi tin nhắn
     */
    TinNhanResponse sendMessage(Long userId, GuiTinNhanDTO dto) throws Exception;

    /**
     * Lấy tin nhắn trong phòng chat
     */
    Page<TinNhanResponse> getMessages(Long userId, Long phongChatId, Pageable pageable) throws Exception;

    /**
     * Lấy tin nhắn cũ hơn
     */
    Page<TinNhanResponse> getMessagesBefore(Long userId, Long phongChatId, Long beforeMessageId, Pageable pageable) throws Exception;

    /**
     * Tìm kiếm tin nhắn
     */
    Page<TinNhanResponse> searchMessages(Long userId, Long phongChatId, String keyword, Pageable pageable) throws Exception;

    /**
     * Chỉnh sửa tin nhắn
     */
    TinNhanResponse editMessage(Long userId, Long messageId, String noiDung) throws Exception;

    /**
     * Xóa tin nhắn
     */
    void deleteMessage(Long userId, Long messageId) throws Exception;

    /**
     * Ghim/bỏ ghim tin nhắn
     */
    TinNhanResponse togglePinMessage(Long userId, Long messageId) throws Exception;

    /**
     * Lấy tin nhắn được ghim
     */
    List<TinNhanResponse> getPinnedMessages(Long userId, Long phongChatId) throws Exception;

    /**
     * Đánh dấu đã đọc
     */
    void markAsRead(Long userId, Long phongChatId) throws Exception;
}
