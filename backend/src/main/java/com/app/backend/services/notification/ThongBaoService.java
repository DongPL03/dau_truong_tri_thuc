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

        thongBaoRepository.save(tb);
    }
}
