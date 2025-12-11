package com.app.backend.services.banbe;

import com.app.backend.dtos.FriendRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.KetBan;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.constant.TrangThaiKetBan;
import com.app.backend.repositories.IKetBanRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.responses.banbe.FriendRequestItemResponse;
import com.app.backend.responses.banbe.FriendSummaryResponse;
import com.app.backend.services.notification.ThongBaoService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;

@Service
@RequiredArgsConstructor
public class BanBeService implements IBanBeService {

    private final IKetBanRepository ketBanRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ThongBaoService thongBaoService;   // 👈 dùng service thông báo, không dùng repo trực tiếp

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
}
