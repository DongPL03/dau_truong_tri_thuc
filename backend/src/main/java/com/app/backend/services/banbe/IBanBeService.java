package com.app.backend.services.banbe;

import com.app.backend.dtos.BlockUserDTO;
import com.app.backend.dtos.FriendRequestDTO;
import com.app.backend.responses.banbe.BlockedUserResponse;
import com.app.backend.responses.banbe.FriendRequestItemResponse;
import com.app.backend.responses.banbe.FriendSuggestionResponse;
import com.app.backend.responses.banbe.FriendSummaryResponse;

import java.util.List;

public interface IBanBeService {

    void sendFriendRequest(Long currentUserId, FriendRequestDTO dto) throws Exception;

    void acceptRequest(Long currentUserId, Long requestId) throws Exception;

    void declineRequest(Long currentUserId, Long requestId) throws Exception;

    void cancelRequest(Long currentUserId, Long requestId) throws Exception;

    void unfriend(Long currentUserId, Long friendUserId) throws Exception;

    List<FriendRequestItemResponse> getIncomingRequests(Long currentUserId);

    List<FriendRequestItemResponse> getOutgoingRequests(Long currentUserId);

    List<FriendSummaryResponse> getFriends(Long currentUserId);

    // ============== BLOCK ==============

    /**
     * Chặn một người dùng
     */
    void blockUser(Long currentUserId, BlockUserDTO dto) throws Exception;

    /**
     * Bỏ chặn một người dùng
     */
    void unblockUser(Long currentUserId, Long targetUserId) throws Exception;

    /**
     * Lấy danh sách người dùng đã chặn
     */
    List<BlockedUserResponse> getBlockedUsers(Long currentUserId);

    /**
     * Kiểm tra xem 2 user có chặn nhau không
     */
    boolean isBlocked(Long userId1, Long userId2);

    // ============== SUGGESTIONS ==============

    /**
     * Gợi ý kết bạn
     */
    List<FriendSuggestionResponse> getFriendSuggestions(Long currentUserId, int limit);

    // ============== SEARCH ==============

    /**
     * Tìm kiếm người dùng theo tên
     */
    List<FriendSummaryResponse> searchUsers(Long currentUserId, String keyword, int limit);

    // ============== STATUS ==============

    /**
     * Kiểm tra trạng thái quan hệ giữa 2 user
     * Returns: "NONE", "FRIEND", "PENDING_SENT", "PENDING_RECEIVED", "BLOCKED_BY_ME", "BLOCKED_BY_THEM"
     */
    String getRelationshipStatus(Long currentUserId, Long targetUserId);
}
