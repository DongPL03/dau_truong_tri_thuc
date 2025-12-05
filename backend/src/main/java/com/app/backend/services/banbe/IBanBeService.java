package com.app.backend.services.banbe;

import com.app.backend.dtos.FriendRequestDTO;
import com.app.backend.responses.banbe.FriendRequestItemResponse;
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
}
