package com.app.backend.services.bocauhoi;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.models.constant.CheDoHienThi;
import com.app.backend.models.constant.TrangThaiBoCauHoi;
import com.app.backend.repositories.*;
import com.app.backend.services.notification.IThongBaoService;
import com.app.backend.models.constant.LoaiThongBao;
import com.app.backend.models.constant.AchievementCode;
import com.app.backend.responses.bocauhoi.UnlockBoCauHoiResponse;
import com.app.backend.services.thanhtich.IThanhTichService;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class BoCauHoiService implements IBoCauHoiService {
    private final IBoCauHoiRepository boCauHoiRepository;
    private final IChuDeRepository chuDeRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final IBoCauHoiMoKhoaRepository boCauHoiMoKhoaRepository;
    private final IBangXepHangRepository bangXepHangRepository;
    private final IVaiTroRepository vaiTroRepository;
    private final IThongBaoService thongBaoService;
    private final ICauHoiRepository cauHoiRepository;
    private final SecurityUtils securityUtils;
    private final IThanhTichService thanhTichService;
    private static final long MIN_QUESTIONS_FOR_APPROVE = 5L;


    @Override
    public BoCauHoi create(BoCauHoiDTO boCauHoiDTO, Long currentUserId) throws DataNotFoundException, PermissionDenyException {
        ChuDe chuDe = chuDeRepository.findById(boCauHoiDTO.getChuDeId())
                .orElseThrow(() -> new DataNotFoundException("Chủ đề không tồn tại"));

        NguoiDung taoBoi = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // 1. Chỉ cần lấy vai trò một lần và chuyển về chữ thường để so sánh
        String role = taoBoi.getVaiTro().getTenVaiTro().toLowerCase();

        // 2. Kiểm tra quyền
        if (!List.of("user", "admin").contains(role)) {
            throw new PermissionDenyException("Bạn không có quyền tạo bộ câu hỏi");
        }
        String trangThaiMoi;
        if (role.equals("admin")) {
            // Admin tạo thì luôn được duyệt
            trangThaiMoi = TrangThaiBoCauHoi.DA_DUYET;
        } else {
            // User tạo thì xét chế độ hiển thị
            // Giả sử CheDoHienThi là một enum và có giá trị .PRIVATE
            if (Objects.equals(boCauHoiDTO.getCheDoHienThi(), CheDoHienThi.PRIVATE)) {
                // User tạo riêng tư -> duyệt luôn
                trangThaiMoi = TrangThaiBoCauHoi.DA_DUYET;
            } else {
                // User tạo công khai (hoặc chế độ khác) -> chờ duyệt
                trangThaiMoi = TrangThaiBoCauHoi.CHO_DUYET;
            }
        }

        boolean isOfficial = false; // Mặc định không phải official khi tạo mới
        String loaiSuDung = "PRACTICE_ONLY"; // Mặc định là PRACTICE_ONLY

        if (role.equals("admin")) {
            // Admin có thể chỉ định loai_su_dung từ DTO, nếu không có thì mặc định RANKED_ONLY
            if (boCauHoiDTO.getLoaiSuDung() != null && !boCauHoiDTO.getLoaiSuDung().isEmpty()) {
                loaiSuDung = boCauHoiDTO.getLoaiSuDung();
            } else {
                loaiSuDung = "RANKED_ONLY"; // Mặc định cho admin
            }

            // Admin tạo thì mặc định official nếu là RANKED_ONLY, ngược lại thì không
            isOfficial = "RANKED_ONLY".equals(loaiSuDung);
        }

        // 4. Xây dựng đối tượng: mặc định FREE, không cần mở khoá
        // Lưu ý: muonTaoTraPhi được lưu để admin biết user muốn tạo trả phí hay free
        BoCauHoi boCauHoi = BoCauHoi.builder()
                .tieuDe(boCauHoiDTO.getTieuDe())
                .moTa(boCauHoiDTO.getMoTa())
                .chuDe(chuDe)
                .cheDoHienThi(boCauHoiDTO.getCheDoHienThi())
                .isOfficial(isOfficial)
                .loaiSuDung(loaiSuDung)
                .isXoa(false)
                .taoBoi(taoBoi)
                .canMoKhoa(false) // Mặc định false, admin sẽ set khi duyệt nếu muonTaoTraPhi = true
                .giaMoKhoa(0L) // Mặc định 0, admin sẽ set khi duyệt nếu muonTaoTraPhi = true
                .muonTaoTraPhi(boCauHoiDTO.getMuonTaoTraPhi() != null ? boCauHoiDTO.getMuonTaoTraPhi() : false)
                .trangThai(trangThaiMoi) // Sử dụng trạng thái đã được xác định ở trên
                .build();

        return boCauHoiRepository.save(boCauHoi);
    }

    @Override
    public List<BoCauHoi> getBattleSetsRankedForCurrentUser() throws Exception {
        return boCauHoiRepository.findRankedBattleSets();
    }

    @Override
    public List<BoCauHoi> getBattleSetsCasualForCurrentUser() throws Exception {
        Long userId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        return boCauHoiRepository.findCasualBattleSets(userId, isAdmin);
    }


    @Override
    @Transactional
    public BoCauHoi update(Long id,
                           BoCauHoiDTO boCauHoiDTO,
                           Long currentUserId,
                           boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id).orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (!isAdmin && !boCauHoi.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được sửa bộ câu hỏi của mình");
        }

        if (boCauHoiDTO.getChuDeId() != null) {
            ChuDe chuDe = chuDeRepository.findById(boCauHoiDTO.getChuDeId())
                    .orElseThrow(() -> new DataNotFoundException("Chủ đề không tồn tại"));
            boCauHoi.setChuDe(chuDe);
        }

        if (boCauHoiDTO.getTieuDe() != null) boCauHoi.setTieuDe(boCauHoiDTO.getTieuDe());
        if (boCauHoiDTO.getMoTa() != null) boCauHoi.setMoTa(boCauHoiDTO.getMoTa());
        if (boCauHoiDTO.getCheDoHienThi() != null) boCauHoi.setCheDoHienThi(boCauHoiDTO.getCheDoHienThi());

        // Cập nhật loai_su_dung (chỉ admin mới có thể chỉnh sửa)
        if (isAdmin && boCauHoiDTO.getLoaiSuDung() != null && !boCauHoiDTO.getLoaiSuDung().isEmpty()) {
            boCauHoi.setLoaiSuDung(boCauHoiDTO.getLoaiSuDung());
            // Nếu admin đổi loai_su_dung sang RANKED_ONLY thì set isOfficial = true, ngược lại = false
            if ("RANKED_ONLY".equals(boCauHoiDTO.getLoaiSuDung())) {
                boCauHoi.setIsOfficial(true);
            } else {
                boCauHoi.setIsOfficial(false);
            }
        }

        // Cập nhật muon_tao_tra_phi (chỉ admin mới có thể chỉnh sửa)
        if (isAdmin && boCauHoiDTO.getMuonTaoTraPhi() != null) {
            boCauHoi.setMuonTaoTraPhi(boCauHoiDTO.getMuonTaoTraPhi());
            // Nếu đổi từ trả phí sang miễn phí, reset giá
            if (!boCauHoiDTO.getMuonTaoTraPhi()) {
                boCauHoi.setCanMoKhoa(false);
                boCauHoi.setGiaMoKhoa(0L);
            }
        }

        // cập nhật lại trạng thái chờ duyệt khi user sửa (tuỳ yêu cầu)
        if (!isAdmin) {
            boCauHoi.setTrangThai(TrangThaiBoCauHoi.CHO_DUYET);
            boCauHoi.setLyDoTuChoi(null);
        }
        return boCauHoiRepository.save(boCauHoi);
    }

    @Override
    @Transactional
    public void delete(Long id,
                       Long currentUserId,
                       boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (!isAdmin && !boCauHoi.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được xoá bộ câu hỏi của mình");
        }
        boCauHoiRepository.delete(boCauHoi); // ON DELETE CASCADE sẽ xoá câu hỏi
    }

    @Override
    public BoCauHoi getById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (!isAdmin) {
            boolean isOwner = boCauHoi.getTaoBoi() != null && boCauHoi.getTaoBoi().getId().equals(currentUserId);
            boolean isPublic = Objects.equals(boCauHoi.getCheDoHienThi(), CheDoHienThi.PUBLIC);
            boolean isApproved = Objects.equals(boCauHoi.getTrangThai(), TrangThaiBoCauHoi.DA_DUYET);

            // Nếu không phải owner và không phải PUBLIC → chặn
            if (!isOwner && !isPublic) {
                throw new PermissionDenyException("Bạn không có quyền xem bộ câu hỏi này");
            }

            // Nếu PUBLIC nhưng chưa được duyệt và không phải owner → chặn
            if (isPublic && !isApproved && !isOwner) {
                throw new PermissionDenyException("Bộ câu hỏi này chưa được duyệt");
            }
        }
        return boCauHoi;
    }

    @Override
    public Page<BoCauHoi> findAllBoCauHoi(PageRequest pageRequest,
                                          String keyword,
                                          Long chuDeId,
                                          String cheDoHienThi,
                                          String trangThai,
                                          String loaiSuDung,
                                          Boolean muonTaoTraPhi,
                                          Long nguoiTaoId,
                                          Long creatorId,
                                          boolean isAdmin) {
        Page<BoCauHoi> page = boCauHoiRepository.searchBoCauHoi(
                pageRequest,
                keyword,
                chuDeId,
                cheDoHienThi,
                trangThai,
                loaiSuDung,
                muonTaoTraPhi,
                nguoiTaoId,
                creatorId,
                isAdmin
        );
        return page;
    }

    @Override
    @Transactional
    public BoCauHoi approve(Long id, Long adminId) {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Bộ câu hỏi không tồn tại"));

        // ✅ Chỉ cho duyệt khi có đủ câu hỏi
        if (boCauHoi.getSoCauHoi() == 0 || boCauHoi.getSoCauHoi() < MIN_QUESTIONS_FOR_APPROVE) {
            throw new IllegalArgumentException(
                    "Bộ câu hỏi phải có ít nhất " + MIN_QUESTIONS_FOR_APPROVE + " câu hỏi mới được duyệt"
            );
        }

        boCauHoi.setTrangThai(TrangThaiBoCauHoi.DA_DUYET);
        boCauHoi.setLyDoTuChoi(null);

        // Quyết định giá mở khóa dựa trên ý muốn của user
        // Nếu user muốn tạo trả phí (muonTaoTraPhi = true), admin sẽ set giá
        // Nếu user muốn tạo miễn phí (muonTaoTraPhi = false), giữ nguyên free
        if (Boolean.TRUE.equals(boCauHoi.getMuonTaoTraPhi())) {
            // User muốn tạo trả phí -> admin set giá dựa trên số câu hỏi
            long suggestedPrice = suggestGiaMoKhoa(boCauHoi);
            if (suggestedPrice > 0) {
                boCauHoi.setCanMoKhoa(true);
                boCauHoi.setGiaMoKhoa(suggestedPrice);
            } else {
                // Nếu không đủ câu hỏi để gợi ý giá, vẫn set giá tối thiểu
                boCauHoi.setCanMoKhoa(true);
                boCauHoi.setGiaMoKhoa(50L); // Giá tối thiểu
            }
        } else {
            // User muốn tạo miễn phí -> giữ nguyên free
            boCauHoi.setCanMoKhoa(false);
            boCauHoi.setGiaMoKhoa(0L);
        }

        BoCauHoi saved = boCauHoiRepository.save(boCauHoi);

        // Gửi thông báo cho người tạo bộ câu hỏi
        if (saved.getTaoBoi() != null) {
            Long creatorId = saved.getTaoBoi().getId();
            String title = saved.getTieuDe();
            String noiDung;
            String metadataJson;
            if (saved.getCanMoKhoa() && saved.getGiaMoKhoa() != null && saved.getGiaMoKhoa() > 0) {
                noiDung = "Bộ câu hỏi \"" + title + "\" của bạn đã được duyệt với giá mở khóa "
                        + saved.getGiaMoKhoa() + " vàng. Mỗi lần người chơi mở khóa, bạn sẽ nhận 70% số vàng này.";
                metadataJson = "{"
                        + "\"type\":\"" + LoaiThongBao.QUIZ_APPROVED + "\","
                        + "\"bo_cau_hoi_id\":" + saved.getId() + ","
                        + "\"gia_mo_khoa\":" + saved.getGiaMoKhoa()
                        + "}";
            } else {
                noiDung = "Bộ câu hỏi \"" + title + "\" của bạn đã được duyệt và phát hành miễn phí.";
                metadataJson = "{"
                        + "\"type\":\"" + LoaiThongBao.QUIZ_APPROVED + "\","
                        + "\"bo_cau_hoi_id\":" + saved.getId() + ","
                        + "\"gia_mo_khoa\":0"
                        + "}";
            }

            // adminId là người duyệt, creatorId là người nhận thông báo
            // Cột "loai" trong DB đang là ENUM(FRIEND_REQUEST,BATTLE_INVITE,SYSTEM),
            // nên ta dùng SYSTEM, còn type chi tiết nằm trong metadata.type = QUIZ_APPROVED.
            thongBaoService.createNotification(
                    adminId,
                    creatorId,
                    LoaiThongBao.SYSTEM,
                    noiDung,
                    metadataJson
            );
        }

        return saved;
    }


    @Override
    @Transactional
    public BoCauHoi reject(Long id, TuChoiBoCauHoiDTO lyDoTuChoi, Long adminId) {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Bộ câu hỏi không tồn tại"));
        boCauHoi.setTrangThai(TrangThaiBoCauHoi.TU_CHOI);
        boCauHoi.setLyDoTuChoi(lyDoTuChoi.getLyDoTuChoi());
        return boCauHoiRepository.save(boCauHoi);
    }

    @Override
    @Transactional
    public BoCauHoi markOfficial(Long id, Long adminId) throws DataNotFoundException, PermissionDenyException {
        // 1. Lấy bộ câu hỏi
        BoCauHoi bo = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // 2. Kiểm tra admin
        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        String role = admin.getVaiTro().getTenVaiTro().toLowerCase();
        if (!role.equals("admin")) {
            throw new PermissionDenyException("Chỉ admin mới có quyền gắn Official");
        }

        // 3. Kiểm tra trạng thái & số câu hỏi
        if (!TrangThaiBoCauHoi.DA_DUYET.equals(bo.getTrangThai())) {
            throw new IllegalArgumentException("Chỉ gắn Official cho bộ đã được duyệt");
        }

        if (bo.getSoCauHoi() < 5) {
            throw new IllegalArgumentException("Cần ít nhất 5 câu hỏi để gắn Official");
        }

        // 4. Gắn cờ official + luôn PRIVATE
        bo.setIsOfficial(true);
        bo.setCheDoHienThi(CheDoHienThi.PRIVATE);

        return boCauHoiRepository.save(bo);
    }

    @Override
    @Transactional
    public BoCauHoi disMarkOfficial(Long id, Long adminId) throws DataNotFoundException, PermissionDenyException {
        // 1. Lấy bộ câu hỏi
        BoCauHoi bo = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // 2. Kiểm tra admin
        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        String role = admin.getVaiTro().getTenVaiTro().toLowerCase();
        if (!role.equals("admin")) {
            throw new PermissionDenyException("Chỉ admin mới có quyền gắn Official");
        }

        // 4. Gắn cờ official + luôn PRIVATE
        bo.setIsOfficial(false);
        return boCauHoiRepository.save(bo);
    }

    @Override
    public Page<BoCauHoi> findPracticeSets(PageRequest pageRequest,
                                           Long creatorId,
                                           boolean isAdmin) {
        return boCauHoiRepository.findPracticeSets(pageRequest, creatorId, isAdmin);
    }

    @Override
    public Page<BoCauHoi> findBattleSets(PageRequest pageRequest) {
        return boCauHoiRepository.findBattleSets(pageRequest);
    }


    @Override
    @Transactional
    public void softDelete(Long id, Long userId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (!isAdmin && !boCauHoi.getTaoBoi().getId().equals(userId)) {
            throw new PermissionDenyException("Bạn không thể xóa bộ câu hỏi của người khác");
        }

        boCauHoi.setIsXoa(true);
        boCauHoiRepository.save(boCauHoi);
    }

    @Override
    @Transactional(readOnly = true)
    public Map<String, Object> thongKeBoCauHoi(Long id) throws DataNotFoundException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        long tongCauHoi = boCauHoi.getSoCauHoi();
        long soNguoiLuyen = phienLuyenTapRepository.countDistinctUsersByBoCauHoi(id);
        Double diemTb = phienLuyenTapRepository.avgScoreByBoCauHoi(id);
        if (diemTb == null) diemTb = 0.0;

        return Map.of(
                "id", boCauHoi.getId(),
                "tieu_de", boCauHoi.getTieuDe(),
                "chu_de", boCauHoi.getChuDe() != null ? boCauHoi.getChuDe().getTen() : null,
                "tong_cau_hoi", tongCauHoi,
                "so_nguoi_luyen", soNguoiLuyen,
                "diem_trung_binh", diemTb
        );
    }

    @Override
    @Transactional
    public UnlockBoCauHoiResponse unlockBoCauHoi(Long boCauHoiId, Long userId) throws Exception {
        BoCauHoi bo = boCauHoiRepository.findById(boCauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // Admin bypass: không cần trừ vàng, return luôn
        if (securityUtils.isAdmin()) {
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));
            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
            return UnlockBoCauHoiResponse.builder()
                    .boCauHoiId(boCauHoiId)
                    .giaMoKhoa(bo.getGiaMoKhoa() != null ? bo.getGiaMoKhoa() : 0L)
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(true)
                    .build();
        }

        // Chủ bộ bypass: không cần trừ vàng, return luôn
        if (bo.getTaoBoi() != null && bo.getTaoBoi().getId().equals(userId)) {
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));
            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
            return UnlockBoCauHoiResponse.builder()
                    .boCauHoiId(boCauHoiId)
                    .giaMoKhoa(bo.getGiaMoKhoa() != null ? bo.getGiaMoKhoa() : 0L)
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(true)
                    .build();
        }

        if (Boolean.FALSE.equals(bo.getCanMoKhoa()) || bo.getGiaMoKhoa() == null || bo.getGiaMoKhoa() <= 0) {
            throw new IllegalStateException("Bộ câu hỏi này không cần mở khóa bằng vàng");
        }

        // đã mở trước đó?
        Optional<BoCauHoiMoKhoa> existed = boCauHoiMoKhoaRepository
                .findByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoiId);
        if (existed.isPresent()) {
            // không trừ vàng nữa, chỉ trả về info
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));

            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;

            return UnlockBoCauHoiResponse.builder()
                    .boCauHoiId(boCauHoiId)
                    .giaMoKhoa(bo.getGiaMoKhoa())
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(true)
                    .build();
        }

        // Lấy BXH để kiểm tra vàng
        BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));

        long currentGold = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
        long price = bo.getGiaMoKhoa();

        if (currentGold < price) {
            throw new IllegalStateException("Không đủ vàng để mở khóa bộ câu hỏi này");
        }

        // trừ vàng
        long afterGold = currentGold - price;
        bxh.setTienVang(afterGold);
        bangXepHangRepository.save(bxh);

        // Chia thưởng cho creator (70%) nếu hội đủ điều kiện:
        // - Bộ có tác giả
        // - Tác giả là USER (không phải ADMIN)
        // - Người đang mở khóa không phải tác giả
        // - Người đang mở khóa không phải admin
        // Nếu tác giả chưa có bảng xếp hạng thì bỏ qua việc thưởng, không ném lỗi để tránh làm fail unlock.
        if (bo.getTaoBoi() != null && !securityUtils.isAdmin()) {
            NguoiDung creator = bo.getTaoBoi();
            String creatorRole = creator.getVaiTro() != null ? creator.getVaiTro().getTenVaiTro().toLowerCase() : "";

            if ("user".equals(creatorRole) && !creator.getId().equals(userId)) {
                Long creatorId = creator.getId();

                bangXepHangRepository.findByNguoiDung_Id(creatorId).ifPresent(creatorBx -> {
                    long reward = Math.round(price * 0.7);
                    long creatorGold = creatorBx.getTienVang() != null ? creatorBx.getTienVang() : 0L;
                    creatorBx.setTienVang(creatorGold + reward);
                    bangXepHangRepository.save(creatorBx);

                    // Gửi thông báo cho creator
                    try {
                        NguoiDung unlocker = nguoiDungRepository.findById(userId)
                                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
                        String unlockerName = unlocker.getHoTen() != null && !unlocker.getHoTen().isBlank()
                                ? unlocker.getHoTen()
                                : unlocker.getTenDangNhap();

                        String noiDung = "Người chơi " + unlockerName + " đã mở khóa bộ câu hỏi \""
                                + bo.getTieuDe() + "\" của bạn và bạn nhận được " + reward + " vàng.";

                        String metadataJson = "{"
                                + "\"type\":\"" + LoaiThongBao.QUIZ_UNLOCKED + "\","
                                + "\"bo_cau_hoi_id\":" + bo.getId() + ","
                                + "\"gold_reward\":" + reward + ","
                                + "\"unlocker_id\":" + userId + ","
                                + "\"unlocker_name\":\"" + unlockerName.replace("\"", "\\\"") + "\""
                                + "}";

                        // Dùng loại SYSTEM, type chi tiết nằm trong metadata.type = QUIZ_UNLOCKED
                        thongBaoService.createNotification(
                                userId,
                                creatorId,
                                LoaiThongBao.SYSTEM,
                                noiDung,
                                metadataJson
                        );
                    } catch (Exception ignored) {
                        // Không để lỗi thông báo làm fail giao dịch unlock
                    }
                });
            }
        }

        // lưu record đã mở khóa
        BoCauHoiMoKhoa record = BoCauHoiMoKhoa.builder()
                .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                .boCauHoi(bo)
                .moKhoaLuc(Instant.now())
                .build();
        boCauHoiMoKhoaRepository.save(record);
        // Flush để đảm bảo data được ghi vào DB ngay, tránh vấn đề transaction isolation
        boCauHoiMoKhoaRepository.flush();

        return UnlockBoCauHoiResponse.builder()
                .boCauHoiId(boCauHoiId)
                .giaMoKhoa(price)
                .tienVangTruoc(currentGold)
                .tienVangSau(afterGold)
                .daMoKhoaTruocDo(false)
                .build();
    }

    /**
     * Gợi ý giá mở khóa dựa trên số câu hỏi.
     * Có thể mở rộng thêm tiêu chí sau này.
     */
    private long suggestGiaMoKhoa(BoCauHoi boCauHoi) {
        int soCau = boCauHoi.getSoCauHoi();
        if (soCau < 20) return 50L;
        if (soCau < 50) return 100L;
        return 150L;
    }

    @Override
    public boolean hasUserUnlockedBo(Long boCauHoiId, Long userId) {
        if (userId == null) {
            return false;
        }
        // Admin bypass unlock
        if (securityUtils.isAdmin()) {
            return true;
        }
        // Chủ bộ bypass unlock
        BoCauHoi bo = boCauHoiRepository.findById(boCauHoiId).orElse(null);
        if (bo != null && bo.getTaoBoi() != null && bo.getTaoBoi().getId().equals(userId)) {
            return true;
        }
        // User thường: check record unlock
        return boCauHoiMoKhoaRepository.existsByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoiId);
    }

    @Override
    @Transactional
    public BoCauHoi duplicate(Long id, Long currentUserId, String loaiSuDung, String purpose) 
            throws DataNotFoundException, PermissionDenyException {
        // Lấy bộ câu hỏi gốc
        BoCauHoi original = boCauHoiRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // Kiểm tra quyền (chỉ admin mới có thể duplicate)
        NguoiDung currentUser = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        String role = currentUser.getVaiTro().getTenVaiTro().toLowerCase();
        if (!"admin".equals(role)) {
            throw new PermissionDenyException("Chỉ admin mới có thể duplicate bộ câu hỏi");
        }

        // Validate loaiSuDung và purpose
        if (loaiSuDung == null || (!loaiSuDung.equals("COURSE_ONLY") && !loaiSuDung.equals("RANKED_ONLY"))) {
            throw new IllegalStateException("loaiSuDung phải là COURSE_ONLY hoặc RANKED_ONLY");
        }
        if (purpose == null || (!purpose.equals("COURSE") && !purpose.equals("RANKED"))) {
            throw new IllegalStateException("purpose phải là COURSE hoặc RANKED");
        }

        // Lấy thông tin creator của bộ câu hỏi gốc
        NguoiDung originalCreator = original.getTaoBoi();
        String creatorName = originalCreator.getHoTen() != null && !originalCreator.getHoTen().isBlank()
                ? originalCreator.getHoTen()
                : originalCreator.getTenDangNhap();

        // Tạo bộ câu hỏi mới (copy)
        BoCauHoi duplicated = BoCauHoi.builder()
                .tieuDe(original.getTieuDe() + " (Copy)")
                .moTa(original.getMoTa())
                .chuDe(original.getChuDe())
                .cheDoHienThi(original.getCheDoHienThi())
                .isOfficial(purpose.equals("RANKED")) // Nếu là RANKED thì set official = true
                .loaiSuDung(loaiSuDung) // Set loaiSuDung theo tham số
                .isXoa(false)
                .taoBoi(currentUser) // Admin là người tạo bộ câu hỏi duplicate
                .canMoKhoa(false) // Copy mặc định free
                .giaMoKhoa(0L)
                .muonTaoTraPhi(false) // Copy mặc định free
                .trangThai(TrangThaiBoCauHoi.DA_DUYET) // Admin duplicate thì tự động duyệt
                .build();

        BoCauHoi saved = boCauHoiRepository.save(duplicated);

        // Copy tất cả câu hỏi
        List<CauHoi> originalQuestions = cauHoiRepository.findByBoCauHoi_Id(id);
        for (CauHoi originalQ : originalQuestions) {
            CauHoi duplicatedQ = CauHoi.builder()
                    .boCauHoi(saved)
                    .noiDung(originalQ.getNoiDung())
                    .loaiNoiDung(originalQ.getLoaiNoiDung())
                    .duongDanTep(originalQ.getDuongDanTep())
                    .dapAnDung(originalQ.getDapAnDung())
                    .luaChonA(originalQ.getLuaChonA())
                    .luaChonB(originalQ.getLuaChonB())
                    .luaChonC(originalQ.getLuaChonC())
                    .luaChonD(originalQ.getLuaChonD())
                    .giaiThich(originalQ.getGiaiThich())
                    .doKho(originalQ.getDoKho())
                    .build();
            cauHoiRepository.save(duplicatedQ);
        }

        // Gửi thông báo broadcast cho toàn server
        String notificationType = purpose.equals("COURSE") 
                ? LoaiThongBao.QUIZ_SELECTED_FOR_COURSE 
                : LoaiThongBao.QUIZ_SELECTED_FOR_RANKED;
        
        String noiDung = purpose.equals("COURSE")
                ? "Bộ câu hỏi \"" + original.getTieuDe() + "\" của " + creatorName 
                    + " đã được admin chọn làm bộ câu hỏi khóa học."
                : "Bộ câu hỏi \"" + original.getTieuDe() + "\" của " + creatorName 
                    + " đã được admin chọn làm bộ câu hỏi thi đấu ranked chính thức.";

        String metadataJson = "{"
                + "\"type\":\"" + notificationType + "\","
                + "\"bo_cau_hoi_id\":" + saved.getId() + ","
                + "\"bo_cau_hoi_tieu_de\":\"" + escapeJson(original.getTieuDe()) + "\","
                + "\"creator_id\":" + originalCreator.getId() + ","
                + "\"creator_name\":\"" + escapeJson(creatorName) + "\","
                + "\"purpose\":\"" + purpose + "\""
                + "}";

        thongBaoService.broadcastNotification(
                currentUserId,
                LoaiThongBao.SYSTEM,
                noiDung,
                metadataJson
        );

        // Nếu là RANKED: tặng gold/exp và achievement cho creator
        if (purpose.equals("RANKED")) {
            // Tặng gold và exp
            bangXepHangRepository.findByNguoiDung_Id(originalCreator.getId()).ifPresent(bxh -> {
                long currentGold = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
                int currentExp = bxh.getExp() != null ? bxh.getExp() : 0;
                
                // Tặng 200 gold và 100 exp
                bxh.setTienVang(currentGold + 200L);
                bxh.setExp(currentExp + 100);
                bangXepHangRepository.save(bxh);
            });

            // Unlock achievement
            try {
                thanhTichService.unlockAchievement(originalCreator.getId(), AchievementCode.QUIZ_SELECTED_FOR_RANKED);
            } catch (Exception e) {
                // Ignore nếu đã có achievement này rồi
            }
        } else {
            // Nếu là COURSE: chỉ unlock achievement
            try {
                thanhTichService.unlockAchievement(originalCreator.getId(), AchievementCode.QUIZ_SELECTED_FOR_COURSE);
            } catch (Exception e) {
                // Ignore nếu đã có achievement này rồi
            }
        }

        return saved;
    }

    private String escapeJson(String input) {
        if (input == null) return "";
        return input.replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r");
    }

}
