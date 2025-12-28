package com.app.backend.services.chat;

import com.app.backend.dtos.chat.CapNhatPhongChatDTO;
import com.app.backend.dtos.chat.GuiTinNhanDTO;
import com.app.backend.dtos.chat.TaoPhongChatDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.enums.LoaiPhongChat;
import com.app.backend.models.enums.LoaiTinNhan;
import com.app.backend.models.enums.VaiTroPhongChat;
import com.app.backend.repositories.*;
import com.app.backend.responses.chat.PhongChatResponse;
import com.app.backend.responses.chat.TinNhanResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PhongChatService implements IPhongChatService {

    private final IPhongChatRepository phongChatRepository;
    private final IThanhVienPhongChatRepository thanhVienRepository;
    private final ITinNhanPhongChatRepository tinNhanRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IKetBanRepository ketBanRepository;

    // ============== PHÒNG CHAT ==============

    @Override
    @Transactional
    public PhongChatResponse createPhongChat(Long userId, TaoPhongChatDTO dto) throws Exception {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng"));

        if (dto.getThanhVienIds() == null || dto.getThanhVienIds().isEmpty()) {
            throw new IllegalArgumentException("Phải có ít nhất 1 thành viên");
        }

        // Nếu chỉ có 1 thành viên -> chat 1-1
        if (dto.getThanhVienIds().size() == 1) {
            return getOrCreatePrivateChat(userId, dto.getThanhVienIds().get(0));
        }

        // Group chat: cần >= 2 thành viên khác
        Set<Long> memberIds = new HashSet<>(dto.getThanhVienIds());
        memberIds.add(userId); // Thêm người tạo

        if (memberIds.size() < 3) {
            throw new IllegalArgumentException("Nhóm chat cần ít nhất 3 thành viên");
        }

        // Tạo phòng chat nhóm
        PhongChat phongChat = PhongChat.builder()
                .ten(dto.getTen() != null ? dto.getTen() : "Nhóm chat mới")
                .anhNhom(dto.getAnhNhom())
                .loai(LoaiPhongChat.NHOM)
                .taoBoi(user)
                .thanhVien(new ArrayList<>())
                .build();

        phongChat = phongChatRepository.save(phongChat);

        // Thêm các thành viên
        for (Long memberId : memberIds) {
            NguoiDung member = nguoiDungRepository.findById(memberId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng: " + memberId));

            ThanhVienPhongChat thanhVien = ThanhVienPhongChat.builder()
                    .phongChat(phongChat)
                    .nguoiDung(member)
                    .vaiTro(memberId.equals(userId) ? VaiTroPhongChat.ADMIN : VaiTroPhongChat.THANH_VIEN)
                    .build();

            thanhVienRepository.save(thanhVien);
            phongChat.getThanhVien().add(thanhVien);
        }

        // Tạo tin nhắn hệ thống
        TinNhanPhongChat systemMessage = TinNhanPhongChat.builder()
                .phongChat(phongChat)
                .guiBoi(user)
                .loai(LoaiTinNhan.HE_THONG)
                .noiDung(user.getHoTen() + " đã tạo nhóm")
                .build();
        tinNhanRepository.save(systemMessage);

        updateLastMessage(phongChat, systemMessage);

        return PhongChatResponse.fromEntity(phongChat, userId, 0);
    }

    @Override
    @Transactional
    public PhongChatResponse getOrCreatePrivateChat(Long userId, Long otherUserId) throws Exception {
        if (userId.equals(otherUserId)) {
            throw new IllegalArgumentException("Không thể chat với chính mình");
        }

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng"));
        NguoiDung otherUser = nguoiDungRepository.findById(otherUserId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng"));

        // Tìm phòng chat 1-1 đã có
        return phongChatRepository.findPrivateChat(userId, otherUserId)
                .map(pc -> {
                    long unread = phongChatRepository.countUnreadMessages(pc.getId(), userId);
                    return PhongChatResponse.fromEntity(pc, userId, unread);
                })
                .orElseGet(() -> {
                    // Tạo mới
                    PhongChat phongChat = PhongChat.builder()
                            .loai(LoaiPhongChat.DON)
                            .taoBoi(user)
                            .thanhVien(new ArrayList<>())
                            .build();

                    phongChat = phongChatRepository.save(phongChat);

                    ThanhVienPhongChat tv1 = ThanhVienPhongChat.builder()
                            .phongChat(phongChat)
                            .nguoiDung(user)
                            .vaiTro(VaiTroPhongChat.THANH_VIEN)
                            .build();

                    ThanhVienPhongChat tv2 = ThanhVienPhongChat.builder()
                            .phongChat(phongChat)
                            .nguoiDung(otherUser)
                            .vaiTro(VaiTroPhongChat.THANH_VIEN)
                            .build();

                    thanhVienRepository.save(tv1);
                    thanhVienRepository.save(tv2);
                    phongChat.getThanhVien().add(tv1);
                    phongChat.getThanhVien().add(tv2);

                    return PhongChatResponse.fromEntity(phongChat, userId, 0);
                });
    }

    @Override
    public Page<PhongChatResponse> getPhongChats(Long userId, Pageable pageable) {
        return phongChatRepository.findByNguoiDungId(userId, pageable)
                .map(pc -> {
                    long unread = phongChatRepository.countUnreadMessages(pc.getId(), userId);
                    return PhongChatResponse.fromEntity(pc, userId, unread);
                });
    }

    @Override
    public List<PhongChatResponse> getPinnedPhongChats(Long userId) {
        return phongChatRepository.findPinnedByNguoiDungId(userId).stream()
                .map(pc -> {
                    long unread = phongChatRepository.countUnreadMessages(pc.getId(), userId);
                    return PhongChatResponse.fromEntity(pc, userId, unread);
                })
                .collect(Collectors.toList());
    }

    @Override
    public PhongChatResponse getPhongChatDetail(Long userId, Long phongChatId) throws Exception {
        PhongChat phongChat = phongChatRepository.findById(phongChatId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy phòng chat"));

        // Kiểm tra quyền truy cập
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        long unread = phongChatRepository.countUnreadMessages(phongChatId, userId);
        return PhongChatResponse.fromEntity(phongChat, userId, unread);
    }

    @Override
    @Transactional
    public PhongChatResponse updatePhongChat(Long userId, Long phongChatId, CapNhatPhongChatDTO dto) throws Exception {
        PhongChat phongChat = phongChatRepository.findById(phongChatId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy phòng chat"));

        if (phongChat.getLoai() != LoaiPhongChat.NHOM) {
            throw new IllegalStateException("Chỉ có thể cập nhật nhóm chat");
        }

        // Kiểm tra quyền admin
        ThanhVienPhongChat currentMember = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(phongChatId, userId)
                .orElseThrow(() -> new IllegalStateException("Bạn không phải thành viên"));

        if (currentMember.getVaiTro() != VaiTroPhongChat.ADMIN) {
            throw new IllegalStateException("Chỉ admin mới có thể cập nhật nhóm");
        }

        NguoiDung admin = nguoiDungRepository.findById(userId).orElseThrow();

        // Cập nhật tên
        if (dto.getTen() != null && !dto.getTen().isBlank()) {
            phongChat.setTen(dto.getTen());
            createSystemMessage(phongChat, admin, admin.getHoTen() + " đã đổi tên nhóm thành \"" + dto.getTen() + "\"");
        }

        // Cập nhật ảnh
        if (dto.getAnhNhom() != null) {
            phongChat.setAnhNhom(dto.getAnhNhom());
        }

        // Thêm thành viên
        if (dto.getThemThanhVien() != null && !dto.getThemThanhVien().isEmpty()) {
            for (Long memberId : dto.getThemThanhVien()) {
                if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, memberId)) {
                    NguoiDung member = nguoiDungRepository.findById(memberId)
                            .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng: " + memberId));

                    ThanhVienPhongChat thanhVien = ThanhVienPhongChat.builder()
                            .phongChat(phongChat)
                            .nguoiDung(member)
                            .vaiTro(VaiTroPhongChat.THANH_VIEN)
                            .build();
                    thanhVienRepository.save(thanhVien);

                    createSystemMessage(phongChat, admin, admin.getHoTen() + " đã thêm " + member.getHoTen() + " vào nhóm");
                }
            }
        }

        // Xóa thành viên
        if (dto.getXoaThanhVien() != null && !dto.getXoaThanhVien().isEmpty()) {
            for (Long memberId : dto.getXoaThanhVien()) {
                if (!memberId.equals(userId)) { // Không thể tự xóa mình
                    thanhVienRepository.findByPhongChatIdAndNguoiDungId(phongChatId, memberId)
                            .ifPresent(tv -> {
                                tv.setDaRoi(true);
                                tv.setRoiLuc(Instant.now());
                                thanhVienRepository.save(tv);
                                createSystemMessage(phongChat, admin, admin.getHoTen() + " đã xóa " + tv.getNguoiDung().getHoTen() + " khỏi nhóm");
                            });
                }
            }
        }

        phongChatRepository.save(phongChat);

        long unread = phongChatRepository.countUnreadMessages(phongChatId, userId);
        return PhongChatResponse.fromEntity(phongChat, userId, unread);
    }

    @Override
    @Transactional
    public void leavePhongChat(Long userId, Long phongChatId) throws Exception {
        PhongChat phongChat = phongChatRepository.findById(phongChatId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy phòng chat"));

        ThanhVienPhongChat member = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(phongChatId, userId)
                .orElseThrow(() -> new IllegalStateException("Bạn không phải thành viên"));

        if (phongChat.getLoai() == LoaiPhongChat.DON) {
            throw new IllegalStateException("Không thể rời chat 1-1");
        }

        NguoiDung user = nguoiDungRepository.findById(userId).orElseThrow();

        member.setDaRoi(true);
        member.setRoiLuc(Instant.now());
        thanhVienRepository.save(member);

        createSystemMessage(phongChat, user, user.getHoTen() + " đã rời khỏi nhóm");

        // Nếu là admin duy nhất, chuyển quyền cho người khác
        if (member.getVaiTro() == VaiTroPhongChat.ADMIN) {
            List<ThanhVienPhongChat> admins = thanhVienRepository.findAdminsByPhongChatId(phongChatId);
            if (admins.isEmpty()) {
                List<ThanhVienPhongChat> members = thanhVienRepository.findActiveByPhongChatId(phongChatId);
                if (!members.isEmpty()) {
                    ThanhVienPhongChat newAdmin = members.get(0);
                    newAdmin.setVaiTro(VaiTroPhongChat.ADMIN);
                    thanhVienRepository.save(newAdmin);
                    createSystemMessage(phongChat, user, newAdmin.getNguoiDung().getHoTen() + " đã trở thành quản trị viên");
                }
            }
        }
    }

    @Override
    @Transactional
    public void deletePhongChat(Long userId, Long phongChatId) throws Exception {
        PhongChat phongChat = phongChatRepository.findById(phongChatId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy phòng chat"));

        ThanhVienPhongChat member = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(phongChatId, userId)
                .orElseThrow(() -> new IllegalStateException("Bạn không phải thành viên"));

        if (phongChat.getLoai() == LoaiPhongChat.NHOM && member.getVaiTro() != VaiTroPhongChat.ADMIN) {
            throw new IllegalStateException("Chỉ admin mới có thể xóa nhóm");
        }

        phongChat.setDaXoa(true);
        phongChatRepository.save(phongChat);
    }

    @Override
    @Transactional
    public PhongChatResponse togglePin(Long userId, Long phongChatId) throws Exception {
        ThanhVienPhongChat member = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(phongChatId, userId)
                .orElseThrow(() -> new IllegalStateException("Bạn không phải thành viên"));

        member.setDaGhim(!member.getDaGhim());
        thanhVienRepository.save(member);

        return getPhongChatDetail(userId, phongChatId);
    }

    @Override
    @Transactional
    public PhongChatResponse toggleMute(Long userId, Long phongChatId) throws Exception {
        ThanhVienPhongChat member = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(phongChatId, userId)
                .orElseThrow(() -> new IllegalStateException("Bạn không phải thành viên"));

        member.setDaTatThongBao(!member.getDaTatThongBao());
        thanhVienRepository.save(member);

        return getPhongChatDetail(userId, phongChatId);
    }

    @Override
    public Page<PhongChatResponse> searchPhongChats(Long userId, String keyword, Pageable pageable) {
        return phongChatRepository.searchByKeyword(userId, keyword, pageable)
                .map(pc -> {
                    long unread = phongChatRepository.countUnreadMessages(pc.getId(), userId);
                    return PhongChatResponse.fromEntity(pc, userId, unread);
                });
    }

    @Override
    public long countTotalUnread(Long userId) {
        return phongChatRepository.countTotalUnreadMessages(userId);
    }

    // ============== TIN NHẮN ==============

    @Override
    @Transactional
    public TinNhanResponse sendMessage(Long userId, GuiTinNhanDTO dto) throws Exception {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng"));

        PhongChat phongChat = phongChatRepository.findById(dto.getPhongChatId())
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy phòng chat"));

        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(dto.getPhongChatId(), userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        TinNhanPhongChat.TinNhanPhongChatBuilder builder = TinNhanPhongChat.builder()
                .phongChat(phongChat)
                .guiBoi(user)
                .loai(dto.getLoai() != null ? dto.getLoai() : LoaiTinNhan.VAN_BAN)
                .noiDung(dto.getNoiDung())
                .urlMedia(dto.getUrlMedia())
                .tenFile(dto.getTenFile())
                .kichThuocFile(dto.getKichThuocFile());

        // Reply
        if (dto.getTraLoiChoId() != null) {
            TinNhanPhongChat replyTo = tinNhanRepository.findById(dto.getTraLoiChoId())
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy tin nhắn trả lời"));
            builder.traLoiCho(replyTo);
        }

        TinNhanPhongChat message = builder.build();
        message = tinNhanRepository.save(message);

        updateLastMessage(phongChat, message);

        return TinNhanResponse.fromEntity(message, userId);
    }

    @Override
    public Page<TinNhanResponse> getMessages(Long userId, Long phongChatId, Pageable pageable) throws Exception {
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        return tinNhanRepository.findByPhongChatId(phongChatId, pageable)
                .map(m -> TinNhanResponse.fromEntity(m, userId));
    }

    @Override
    public Page<TinNhanResponse> getMessagesBefore(Long userId, Long phongChatId, Long beforeMessageId, Pageable pageable) throws Exception {
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        TinNhanPhongChat beforeMessage = tinNhanRepository.findById(beforeMessageId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy tin nhắn"));

        return tinNhanRepository.findByPhongChatIdAndGuiLucBefore(phongChatId, beforeMessage.getGuiLuc(), pageable)
                .map(m -> TinNhanResponse.fromEntity(m, userId));
    }

    @Override
    public Page<TinNhanResponse> searchMessages(Long userId, Long phongChatId, String keyword, Pageable pageable) throws Exception {
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        return tinNhanRepository.searchByKeyword(phongChatId, keyword, pageable)
                .map(m -> TinNhanResponse.fromEntity(m, userId));
    }

    @Override
    @Transactional
    public TinNhanResponse editMessage(Long userId, Long messageId, String noiDung) throws Exception {
        TinNhanPhongChat message = tinNhanRepository.findById(messageId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy tin nhắn"));

        if (!message.getGuiBoi().getId().equals(userId)) {
            throw new IllegalStateException("Bạn chỉ có thể sửa tin nhắn của mình");
        }

        if (message.getLoai() != LoaiTinNhan.VAN_BAN) {
            throw new IllegalStateException("Chỉ có thể sửa tin nhắn văn bản");
        }

        message.setNoiDung(noiDung);
        message.setChinhSuaLuc(Instant.now());
        message = tinNhanRepository.save(message);

        return TinNhanResponse.fromEntity(message, userId);
    }

    @Override
    @Transactional
    public void deleteMessage(Long userId, Long messageId) throws Exception {
        TinNhanPhongChat message = tinNhanRepository.findById(messageId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy tin nhắn"));

        // Chỉ người gửi hoặc admin phòng chat có thể xóa
        boolean isOwner = message.getGuiBoi().getId().equals(userId);
        boolean isAdmin = thanhVienRepository
                .findByPhongChatIdAndNguoiDungId(message.getPhongChat().getId(), userId)
                .map(tv -> tv.getVaiTro() == VaiTroPhongChat.ADMIN)
                .orElse(false);

        if (!isOwner && !isAdmin) {
            throw new IllegalStateException("Bạn không có quyền xóa tin nhắn này");
        }

        tinNhanRepository.softDelete(messageId, Instant.now());
    }

    @Override
    @Transactional
    public TinNhanResponse togglePinMessage(Long userId, Long messageId) throws Exception {
        TinNhanPhongChat message = tinNhanRepository.findById(messageId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy tin nhắn"));

        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(message.getPhongChat().getId(), userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        message.setDaGhim(!message.getDaGhim());
        message = tinNhanRepository.save(message);

        return TinNhanResponse.fromEntity(message, userId);
    }

    @Override
    public List<TinNhanResponse> getPinnedMessages(Long userId, Long phongChatId) throws Exception {
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        return tinNhanRepository.findPinnedByPhongChatId(phongChatId).stream()
                .map(m -> TinNhanResponse.fromEntity(m, userId))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public void markAsRead(Long userId, Long phongChatId) throws Exception {
        if (!thanhVienRepository.existsActiveByPhongChatIdAndNguoiDungId(phongChatId, userId)) {
            throw new IllegalStateException("Bạn không phải thành viên của phòng chat này");
        }

        thanhVienRepository.updateDocCuoiLuc(phongChatId, userId, Instant.now());
    }

    // ============== HELPER METHODS ==============

    private void updateLastMessage(PhongChat phongChat, TinNhanPhongChat message) {
        String preview = message.getNoiDung();
        if (message.getLoai() == LoaiTinNhan.HINH_ANH) {
            preview = "📷 Hình ảnh";
        } else if (message.getLoai() == LoaiTinNhan.TAP_TIN) {
            preview = "📎 " + (message.getTenFile() != null ? message.getTenFile() : "Tập tin");
        } else if (message.getLoai() == LoaiTinNhan.AM_THANH) {
            preview = "🎵 Tin nhắn thoại";
        } else if (preview != null && preview.length() > 50) {
            preview = preview.substring(0, 50) + "...";
        }

        phongChat.setTinNhanCuoi(preview);
        phongChat.setThoiGianTinNhanCuoi(message.getGuiLuc());
        phongChatRepository.save(phongChat);
    }

    private void createSystemMessage(PhongChat phongChat, NguoiDung user, String content) {
        TinNhanPhongChat systemMessage = TinNhanPhongChat.builder()
                .phongChat(phongChat)
                .guiBoi(user)
                .loai(LoaiTinNhan.HE_THONG)
                .noiDung(content)
                .build();
        tinNhanRepository.save(systemMessage);
        updateLastMessage(phongChat, systemMessage);
    }
}
