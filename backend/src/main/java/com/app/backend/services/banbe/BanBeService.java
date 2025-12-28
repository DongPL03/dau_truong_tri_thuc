package com.app.backend.services.banbe;

import com.app.backend.dtos.BlockUserDTO;
import com.app.backend.dtos.FriendRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.constant.TrangThaiKetBan;
import com.app.backend.repositories.*;
import com.app.backend.responses.banbe.BlockedUserResponse;
import com.app.backend.responses.banbe.FriendRequestItemResponse;
import com.app.backend.responses.banbe.FriendSuggestionResponse;
import com.app.backend.responses.banbe.FriendSummaryResponse;
import com.app.backend.services.notification.ThongBaoService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class BanBeService implements IBanBeService {

    private final IKetBanRepository ketBanRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IChanNguoiDungRepository chanNguoiDungRepository;
    private final INguoiChoiTranDauRepository nguoiChoiTranDauRepository;
    private final ThongBaoService thongBaoService;
    private final IBangXepHangRepository bangXepHangRepository;

    @Override
    @Transactional
    public void sendFriendRequest(Long currentUserId, FriendRequestDTO dto) throws Exception {
        Long targetId = dto.target_user_id();

        if (currentUserId.equals(targetId)) {
            throw new IllegalArgumentException("Bạn không thể kết bạn với chính mình");
        }

        NguoiDung sender = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người gửi không tồn tại"));

        NguoiDung receiver = nguoiDungRepository.findById(targetId)
                .orElseThrow(() -> new DataNotFoundException("Người nhận không tồn tại"));

        // Đã từng có record kết bạn 2 chiều?
        boolean existed = ketBanRepository.existsByNguoiGui_IdAndNguoiNhan_Id(currentUserId, targetId)
                || ketBanRepository.existsByNguoiGui_IdAndNguoiNhan_Id(targetId, currentUserId);

        if (existed) {
            throw new IllegalStateException("Đã tồn tại quan hệ kết bạn hoặc lời mời giữa 2 người");
        }

        KetBan kb = KetBan.builder()
                .nguoiGui(sender)
                .nguoiNhan(receiver)
                .trangThai(TrangThaiKetBan.PENDING)
                .taoLuc(Instant.now())
                .build();
        ketBanRepository.save(kb);

        // 🔔 Thông báo realtime + lưu DB cho người được mời
        thongBaoService.notifyFriendRequest(sender.getId(), receiver.getId(), kb.getId());
    }

    @Override
    @Transactional
    public void acceptRequest(Long currentUserId, Long requestId) throws Exception {
        KetBan kb = ketBanRepository.findByIdAndNguoiNhan_Id(requestId, currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Lời mời không tồn tại hoặc không thuộc về bạn"));

        if (!TrangThaiKetBan.PENDING.equals(kb.getTrangThai())) {
            throw new IllegalStateException("Lời mời không còn ở trạng thái chờ");
        }

        kb.setTrangThai(TrangThaiKetBan.ACCEPTED);
        ketBanRepository.save(kb);

        Long requesterId = kb.getNguoiGui().getId();
        Long accepterId = kb.getNguoiNhan().getId();

        // 🔔 Thông báo cho người gửi lời mời biết đã được chấp nhận
        thongBaoService.notifyFriendAccepted(accepterId, requesterId, kb.getId());
    }

    @Override
    @Transactional
    public void declineRequest(Long currentUserId, Long requestId) throws Exception {
        KetBan kb = ketBanRepository.findByIdAndNguoiNhan_Id(requestId, currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Lời mời không tồn tại hoặc không thuộc về bạn"));

        if (!TrangThaiKetBan.PENDING.equals(kb.getTrangThai())) {
            return; // coi như đã xử lý rồi, không làm gì thêm
        }

        Long requesterId = kb.getNguoiGui().getId();
        Long declinerId = kb.getNguoiNhan().getId();

        // Xoá lời mời
        ketBanRepository.delete(kb);

        // 🔔 Thông báo cho người gửi là đã bị từ chối
        thongBaoService.notifyFriendDeclined(declinerId, requesterId, requestId);
    }

    @Override
    @Transactional
    public void cancelRequest(Long currentUserId, Long requestId) throws Exception {
        KetBan kb = ketBanRepository.findByIdAndNguoiGui_Id(requestId, currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Lời mời không tồn tại hoặc không thuộc về bạn"));

        if (!TrangThaiKetBan.PENDING.equals(kb.getTrangThai())) {
            throw new IllegalStateException("Chỉ có thể huỷ lời mời đang chờ");
        }
        ketBanRepository.delete(kb);
        // (tuỳ bạn: có thể không cần thông báo gì cho bên kia khi huỷ)
    }

    @Override
    @Transactional
    public void unfriend(Long currentUserId, Long friendUserId) throws Exception {
        // Xoá bất kỳ record ACCEPTED nào giữa 2 user
        List<KetBan> list = ketBanRepository.findFriends(currentUserId);
        list.stream()
                .filter(k ->
                        (k.getNguoiGui().getId().equals(friendUserId)
                                || k.getNguoiNhan().getId().equals(friendUserId)))
                .forEach(ketBanRepository::delete);

        // (tuỳ bạn: có thể thêm 1 thông báo "X đã huỷ kết bạn với bạn" nếu muốn)
    }

    @Override
    @Transactional(readOnly = true)
    public List<FriendRequestItemResponse> getIncomingRequests(Long currentUserId) {
        return ketBanRepository.findIncomingRequests(currentUserId)
                .stream()
                .map(k -> FriendRequestItemResponse.builder()
                        .requestId(k.getId())
                        .nguoiGuiId(k.getNguoiGui().getId())
                        .nguoiGuiTen(k.getNguoiGui().getHoTen())
                        .nguoiNhanId(k.getNguoiNhan().getId())
                        .nguoiNhanTen(k.getNguoiNhan().getHoTen())
                        .trangThai(k.getTrangThai())
                        .taoLuc(k.getTaoLuc() != null ? k.getTaoLuc() : null)
                        .build())
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public List<FriendRequestItemResponse> getOutgoingRequests(Long currentUserId) {
        return ketBanRepository.findOutgoingRequests(currentUserId)
                .stream()
                .map(k -> FriendRequestItemResponse.builder()
                        .requestId(k.getId())
                        .nguoiGuiId(k.getNguoiGui().getId())
                        .nguoiGuiTen(k.getNguoiGui().getHoTen())
                        .nguoiNhanId(k.getNguoiNhan().getId())
                        .nguoiNhanTen(k.getNguoiNhan().getHoTen())
                        .trangThai(k.getTrangThai())
                        .taoLuc(k.getTaoLuc() != null ? k.getTaoLuc() : null)
                        .build())
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public List<FriendSummaryResponse> getFriends(Long currentUserId) {
        return ketBanRepository.findFriends(currentUserId)
                .stream()
                .map(k -> {
                    NguoiDung friend = k.getNguoiGui().getId().equals(currentUserId)
                            ? k.getNguoiNhan()
                            : k.getNguoiGui();
                    return FriendSummaryResponse.builder()
                            .userId(friend.getId())
                            .hoTen(friend.getHoTen())
                            .avatarUrl(friend.getAvatarUrl())
                            .trangThai(friend.getTrangThai() != null ? friend.getTrangThai() : "OFFLINE")
                            .build();
                })
                .toList();
    }

    // ============== BLOCK ==============

    @Override
    @Transactional
    public void blockUser(Long currentUserId, BlockUserDTO dto) throws Exception {
        Long targetId = dto.targetUserId();

        if (currentUserId.equals(targetId)) {
            throw new IllegalArgumentException("Bạn không thể chặn chính mình");
        }

        NguoiDung blocker = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        NguoiDung blocked = nguoiDungRepository.findById(targetId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng cần chặn không tồn tại"));

        // Kiểm tra đã chặn chưa
        if (chanNguoiDungRepository.existsByNguoiChan_IdAndNguoiBiChan_Id(currentUserId, targetId)) {
            throw new IllegalStateException("Bạn đã chặn người dùng này rồi");
        }

        // Xóa quan hệ bạn bè nếu có
        ketBanRepository.findFriends(currentUserId).stream()
                .filter(k -> k.getNguoiGui().getId().equals(targetId) || k.getNguoiNhan().getId().equals(targetId))
                .forEach(ketBanRepository::delete);

        // Xóa lời mời kết bạn đang pending (cả 2 chiều)
        ketBanRepository.findOutgoingRequests(currentUserId).stream()
                .filter(k -> k.getNguoiNhan().getId().equals(targetId))
                .forEach(ketBanRepository::delete);

        ketBanRepository.findIncomingRequests(currentUserId).stream()
                .filter(k -> k.getNguoiGui().getId().equals(targetId))
                .forEach(ketBanRepository::delete);

        // Tạo record chặn
        ChanNguoiDung block = ChanNguoiDung.builder()
                .nguoiChan(blocker)
                .nguoiBiChan(blocked)
                .lyDo(dto.lyDo())
                .build();
        chanNguoiDungRepository.save(block);
    }

    @Override
    @Transactional
    public void unblockUser(Long currentUserId, Long targetUserId) throws Exception {
        ChanNguoiDung block = chanNguoiDungRepository
                .findByNguoiChan_IdAndNguoiBiChan_Id(currentUserId, targetUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa chặn người dùng này"));

        chanNguoiDungRepository.delete(block);
    }

    @Override
    @Transactional(readOnly = true)
    public List<BlockedUserResponse> getBlockedUsers(Long currentUserId) {
        return chanNguoiDungRepository.findAllByNguoiChan(currentUserId)
                .stream()
                .map(c -> BlockedUserResponse.builder()
                        .blockId(c.getId())
                        .userId(c.getNguoiBiChan().getId())
                        .hoTen(c.getNguoiBiChan().getHoTen())
                        .avatarUrl(c.getNguoiBiChan().getAvatarUrl())
                        .lyDo(c.getLyDo())
                        .chanLuc(c.getChanLuc())
                        .build())
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public boolean isBlocked(Long userId1, Long userId2) {
        return chanNguoiDungRepository.isBlocked(userId1, userId2);
    }

    // ============== SUGGESTIONS ==============

    @Override
    @Transactional(readOnly = true)
    public List<FriendSuggestionResponse> getFriendSuggestions(Long currentUserId, int limit) {
        // Lấy danh sách bạn bè hiện tại
        Set<Long> friendIds = ketBanRepository.findFriends(currentUserId).stream()
                .map(k -> k.getNguoiGui().getId().equals(currentUserId)
                        ? k.getNguoiNhan().getId()
                        : k.getNguoiGui().getId())
                .collect(Collectors.toSet());

        // Lấy danh sách người đã chặn hoặc bị chặn
        Set<Long> blockedIds = new HashSet<>();
        blockedIds.addAll(chanNguoiDungRepository.findBlockedUserIds(currentUserId));
        blockedIds.addAll(chanNguoiDungRepository.findBlockerUserIds(currentUserId));

        // Lấy danh sách pending requests
        Set<Long> pendingIds = new HashSet<>();
        ketBanRepository.findOutgoingRequests(currentUserId).forEach(k -> pendingIds.add(k.getNguoiNhan().getId()));
        ketBanRepository.findIncomingRequests(currentUserId).forEach(k -> pendingIds.add(k.getNguoiGui().getId()));

        // Exclude list = friends + blocked + pending + self
        Set<Long> excludeIds = new HashSet<>(friendIds);
        excludeIds.addAll(blockedIds);
        excludeIds.addAll(pendingIds);
        excludeIds.add(currentUserId);

        List<FriendSuggestionResponse> suggestions = new ArrayList<>();

        // 1. Gợi ý từ người chơi cùng trận đấu gần đây (7 ngày)
        Instant sevenDaysAgo = Instant.now().minus(7, ChronoUnit.DAYS);
        List<NguoiChoiTranDau> myRecentBattles = nguoiChoiTranDauRepository.findByNguoiDung_IdAndThamGiaLucAfter(currentUserId, sevenDaysAgo);

        Map<Long, Integer> sameBattleCount = new HashMap<>();
        for (NguoiChoiTranDau myBattle : myRecentBattles) {
            Long tranDauId = myBattle.getTranDau().getId();
            List<NguoiChoiTranDau> battlemates = nguoiChoiTranDauRepository.findAllByTranDau_Id(tranDauId);
            for (NguoiChoiTranDau mate : battlemates) {
                Long mateId = mate.getNguoiDung().getId();
                if (!excludeIds.contains(mateId)) {
                    sameBattleCount.merge(mateId, 1, Integer::sum);
                }
            }
        }

        // Sort by battle count và lấy top
        sameBattleCount.entrySet().stream()
                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                .limit(limit)
                .forEach(entry -> {
                    NguoiDung user = nguoiDungRepository.findById(entry.getKey()).orElse(null);
                    if (user == null || user.isXoa()) return;
                    
                    BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(user.getId()).orElse(null);
                    
                    suggestions.add(FriendSuggestionResponse.builder()
                            .userId(user.getId())
                            .hoTen(user.getHoTen())
                            .avatarUrl(user.getAvatarUrl())
                            .level(bxh != null ? bxh.getLevel() : 1)
                            .tongDiem(bxh != null ? Long.valueOf(bxh.getTongDiem()) : 0L)
                            .reason("SAME_BATTLE")
                            .mutualFriendsCount(entry.getValue())
                            .build());
                    excludeIds.add(user.getId());
                });

        // 2. Nếu chưa đủ, thêm bạn của bạn (mutual friends)
        if (suggestions.size() < limit) {
            Map<Long, Integer> mutualFriendCount = new HashMap<>();
            for (Long friendId : friendIds) {
                List<KetBan> friendsOfFriend = ketBanRepository.findFriends(friendId);
                for (KetBan k : friendsOfFriend) {
                    Long fofId = k.getNguoiGui().getId().equals(friendId)
                            ? k.getNguoiNhan().getId()
                            : k.getNguoiGui().getId();
                    if (!excludeIds.contains(fofId)) {
                        mutualFriendCount.merge(fofId, 1, Integer::sum);
                    }
                }
            }

            mutualFriendCount.entrySet().stream()
                    .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                    .limit(limit - suggestions.size())
                    .forEach(entry -> {
                        NguoiDung user = nguoiDungRepository.findById(entry.getKey()).orElse(null);
                        if (user == null || user.isXoa()) return;
                        
                        BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(user.getId()).orElse(null);
                        
                        suggestions.add(FriendSuggestionResponse.builder()
                                .userId(user.getId())
                                .hoTen(user.getHoTen())
                                .avatarUrl(user.getAvatarUrl())
                                .level(bxh != null ? bxh.getLevel() : 1)
                                .tongDiem(bxh != null ? Long.valueOf(bxh.getTongDiem()) : 0L)
                                .reason("MUTUAL_FRIEND")
                                .mutualFriendsCount(entry.getValue())
                                .build());
                        excludeIds.add(user.getId());
                    });
        }

        // 3. Nếu vẫn chưa đủ, thêm người chơi popular (điểm cao từ BangXepHang)
        if (suggestions.size() < limit) {
            // Lấy tất cả BangXepHang, sort theo tongDiem giảm dần
            bangXepHangRepository.findAll().stream()
                    .filter(bxh -> {
                        NguoiDung u = bxh.getNguoiDung();
                        return u != null && !u.isXoa() && u.isActive() && !excludeIds.contains(u.getId());
                    })
                    .sorted(Comparator.comparing(BangXepHang::getTongDiem, Comparator.nullsLast(Comparator.reverseOrder())))
                    .limit(limit - suggestions.size())
                    .forEach(bxh -> {
                        NguoiDung user = bxh.getNguoiDung();
                        suggestions.add(FriendSuggestionResponse.builder()
                                .userId(user.getId())
                                .hoTen(user.getHoTen())
                                .avatarUrl(user.getAvatarUrl())
                                .level(bxh.getLevel())
                                .tongDiem(Long.valueOf(bxh.getTongDiem()))
                                .reason("POPULAR")
                                .mutualFriendsCount(0)
                                .build());
                    });
        }

        return suggestions;
    }

    // ============== SEARCH ==============

    @Override
    @Transactional(readOnly = true)
    public List<FriendSummaryResponse> searchUsers(Long currentUserId, String keyword, int limit) {
        if (keyword == null || keyword.trim().isEmpty()) {
            return List.of();
        }

        String searchTerm = keyword.trim().toLowerCase();

        // Lấy danh sách blocked
        Set<Long> blockedIds = new HashSet<>();
        blockedIds.addAll(chanNguoiDungRepository.findBlockedUserIds(currentUserId));
        blockedIds.addAll(chanNguoiDungRepository.findBlockerUserIds(currentUserId));

        return nguoiDungRepository.findAll().stream()
                .filter(u -> !u.isXoa() && u.isActive())
                .filter(u -> !u.getId().equals(currentUserId))
                .filter(u -> !blockedIds.contains(u.getId()))
                .filter(u -> {
                    String name = u.getHoTen() != null ? u.getHoTen().toLowerCase() : "";
                    String username = u.getTenDangNhap() != null ? u.getTenDangNhap().toLowerCase() : "";
                    return name.contains(searchTerm) || username.contains(searchTerm);
                })
                .limit(limit)
                .map(u -> FriendSummaryResponse.builder()
                        .userId(u.getId())
                        .hoTen(u.getHoTen())
                        .avatarUrl(u.getAvatarUrl())
                        .trangThai(u.getTrangThai() != null ? u.getTrangThai() : "OFFLINE")
                        .build())
                .toList();
    }

    // ============== STATUS ==============

    @Override
    @Transactional(readOnly = true)
    public String getRelationshipStatus(Long currentUserId, Long targetUserId) {
        // Check blocked
        if (chanNguoiDungRepository.existsByNguoiChan_IdAndNguoiBiChan_Id(currentUserId, targetUserId)) {
            return "BLOCKED_BY_ME";
        }
        if (chanNguoiDungRepository.existsByNguoiChan_IdAndNguoiBiChan_Id(targetUserId, currentUserId)) {
            return "BLOCKED_BY_THEM";
        }

        // Check friends
        if (ketBanRepository.areFriends(currentUserId, targetUserId)) {
            return "FRIEND";
        }

        // Check pending sent
        boolean pendingSent = ketBanRepository.findOutgoingRequests(currentUserId).stream()
                .anyMatch(k -> k.getNguoiNhan().getId().equals(targetUserId));
        if (pendingSent) {
            return "PENDING_SENT";
        }

        // Check pending received
        boolean pendingReceived = ketBanRepository.findIncomingRequests(currentUserId).stream()
                .anyMatch(k -> k.getNguoiGui().getId().equals(targetUserId));
        if (pendingReceived) {
            return "PENDING_RECEIVED";
        }

        return "NONE";
    }
}
