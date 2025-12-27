package com.app.backend.services.notification;

import com.app.backend.components.NotificationWsPublisher;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.ThongBao;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.IThongBaoRepository;
import com.app.backend.responses.notification.NotificationResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ThongBaoService implements IThongBaoService {

    private final IThongBaoRepository thongBaoRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final NotificationWsPublisher notificationWsPublisher;

    @Override
    @Transactional(readOnly = true)
    public Page<NotificationResponse> getMyNotifications(Long currentUserId, PageRequest pageRequest) {
        return thongBaoRepository
                .findByNguoiNhan_IdOrderByTaoLucDesc(currentUserId, pageRequest)
                .map(NotificationResponse::fromEntity);
    }

    @Override
    @Transactional(readOnly = true)
    public long getUnreadCount(Long currentUserId) {
        return thongBaoRepository.countByNguoiNhan_IdAndDaDocFalse(currentUserId);
    }

    @Override
    @Transactional
    public void markRead(Long currentUserId, Long thongBaoId) throws Exception {
        int updated = thongBaoRepository.markReadByIdAndNguoiNhan(thongBaoId, currentUserId);
        if (updated == 0) {
            throw new DataNotFoundException("Thông báo không tồn tại hoặc không thuộc về bạn");
        }
    }

    @Override
    @Transactional
    public void markAllRead(Long currentUserId) {
        thongBaoRepository.markAllReadByNguoiNhan(currentUserId);
    }

    @Override
    @Transactional
    public void createNotification(
            Long nguoiGuiId,
            Long nguoiNhanId,
            String loai,
            String noiDung,
            String metadataJson
    ) {
        NguoiDung nguoiGui = nguoiDungRepository.getReferenceById(nguoiGuiId);
        NguoiDung nguoiNhan = nguoiDungRepository.getReferenceById(nguoiNhanId);

        ThongBao tb = ThongBao.builder()
                .nguoiGui(nguoiGui)
                .nguoiNhan(nguoiNhan)
                .loai(loai)
                .noiDung(noiDung)
                .metadata(metadataJson)
                .daDoc(false)
                .taoLuc(Instant.now())
                .build();

        ThongBao saved = thongBaoRepository.save(tb);

        // 🔔 Bắn realtime qua WebSocket cho người nhận
        NotificationResponse payload = NotificationResponse.fromEntity(saved);
        notificationWsPublisher.publishToUser(nguoiNhanId, payload);
    }

    // =========================================================
    //  FRIEND MODULE – helper cho kết bạn
    // =========================================================

    /**
     * Gửi thông báo khi A gửi lời mời kết bạn cho B
     */
    @Override
    @Transactional
    public void notifyFriendRequest(Long nguoiGuiId, Long nguoiNhanId, Long ketBanId) {
        NguoiDung sender = nguoiDungRepository.getReferenceById(nguoiGuiId);

        String hoTen = sender.getHoTen() != null && !sender.getHoTen().isBlank()
                ? sender.getHoTen()
                : sender.getTenDangNhap();

        String noiDung = hoTen + " đã gửi lời mời kết bạn cho bạn.";

        // metadata JSON đơn giản, đủ dùng cho FE
        String metadataJson =
                "{"
                        + "\"type\":\"FRIEND_REQUEST\","
                        + "\"ket_ban_id\":" + ketBanId + ","
                        + "\"from_user_id\":" + nguoiGuiId + ","
                        + "\"from_ho_ten\":\"" + escapeJson(hoTen) + "\""
                        + "}";

        createNotification(
                nguoiGuiId,
                nguoiNhanId,
                "FRIEND_REQUEST",
                noiDung,
                metadataJson
        );
    }

    /**
     * Gửi thông báo cho người gửi khi lời mời được chấp nhận
     */
    @Override
    @Transactional
    public void notifyFriendAccepted(Long nguoiChapNhanId, Long nguoiGuiLoiMoiId, Long ketBanId) {
        NguoiDung accepter = nguoiDungRepository.getReferenceById(nguoiChapNhanId);

        String hoTen = accepter.getHoTen() != null && !accepter.getHoTen().isBlank()
                ? accepter.getHoTen()
                : accepter.getTenDangNhap();

        String noiDung = hoTen + " đã chấp nhận lời mời kết bạn của bạn.";

        String metadataJson =
                "{"
                        + "\"type\":\"FRIEND_ACCEPTED\","
                        + "\"ket_ban_id\":" + ketBanId + ","
                        + "\"accepter_id\":" + nguoiChapNhanId + ","
                        + "\"accepter_ho_ten\":\"" + escapeJson(hoTen) + "\""
                        + "}";

        createNotification(
                nguoiChapNhanId,
                nguoiGuiLoiMoiId,
                "FRIEND_REQUEST", // vẫn dùng loại FRIEND_REQUEST
                noiDung,
                metadataJson
        );
    }

    /**
     * Gửi thông báo cho người gửi khi lời mời bị từ chối
     */
    @Override
    @Transactional
    public void notifyFriendDeclined(Long nguoiTuChoiId, Long nguoiGuiLoiMoiId, Long ketBanId) {
        NguoiDung decliner = nguoiDungRepository.getReferenceById(nguoiTuChoiId);

        String hoTen = decliner.getHoTen() != null && !decliner.getHoTen().isBlank()
                ? decliner.getHoTen()
                : decliner.getTenDangNhap();

        String noiDung = hoTen + " đã từ chối lời mời kết bạn của bạn.";

        String metadataJson =
                "{"
                        + "\"type\":\"FRIEND_DECLINED\","
                        + "\"ket_ban_id\":" + ketBanId + ","
                        + "\"decliner_id\":" + nguoiTuChoiId + ","
                        + "\"decliner_ho_ten\":\"" + escapeJson(hoTen) + "\""
                        + "}";

        createNotification(
                nguoiTuChoiId,
                nguoiGuiLoiMoiId,
                "FRIEND_REQUEST",
                noiDung,
                metadataJson
        );
    }

    /**
     * Gửi thông báo cho tất cả người dùng trong hệ thống (broadcast)
     */
    @Override
    @Transactional
    public void broadcastNotification(Long nguoiGuiId, String loai, String noiDung, String metadataJson) {
        NguoiDung nguoiGui = nguoiDungRepository.getReferenceById(nguoiGuiId);
        
        // Lấy tất cả người dùng đang hoạt động (không bị xóa, không bị khóa)
        List<NguoiDung> allUsers = nguoiDungRepository.findAll().stream()
                .filter(u -> u.getIsXoa() == null || !u.getIsXoa())
                .filter(u -> u.getIsActive() == null || u.getIsActive())
                .toList();

        // Tạo thông báo cho từng user
        for (NguoiDung user : allUsers) {
            ThongBao tb = ThongBao.builder()
                    .nguoiGui(nguoiGui)
                    .nguoiNhan(user)
                    .loai(loai)
                    .noiDung(noiDung)
                    .metadata(metadataJson)
                    .daDoc(false)
                    .taoLuc(Instant.now())
                    .build();

            ThongBao saved = thongBaoRepository.save(tb);

            // 🔔 Bắn realtime qua WebSocket
            NotificationResponse payload = NotificationResponse.fromEntity(saved);
            notificationWsPublisher.publishToUser(user.getId(), payload);
        }
    }

    /**
     * Helper nhỏ để escape dấu " trong tên cho metadata JSON thủ công
     */
    private String escapeJson(String input) {
        if (input == null) return "";
        return input.replace("\"", "\\\"");
    }
}
