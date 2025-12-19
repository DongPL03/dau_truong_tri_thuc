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
import com.app.backend.responses.bocauhoi.UnlockBoCauHoiResponse;
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
    private final SecurityUtils securityUtils;
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
            isOfficial = true; // Admin tạo thì có thể đánh dấu official
            loaiSuDung = "RANKED_ONLY"; // Admin tạo thì loại sử dụng là BOTH
        }

        // 4. Xây dựng đối tượng
        BoCauHoi boCauHoi = BoCauHoi.builder()
                .tieuDe(boCauHoiDTO.getTieuDe())
                .moTa(boCauHoiDTO.getMoTa())
                .chuDe(chuDe)
                .cheDoHienThi(boCauHoiDTO.getCheDoHienThi())
                .isOfficial(isOfficial)
                .loaiSuDung(loaiSuDung)
                .isXoa(false)
                .taoBoi(taoBoi)
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
        return boCauHoiRepository.findCasualBattleSets(userId);
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
            
            if (!isOwner && !isPublic) {
                throw new PermissionDenyException("Bạn không có quyền xem bộ câu hỏi này");
            }
            
            // ✅ Nếu bộ PUBLIC nhưng cần unlock và user chưa unlock → chặn xem chi tiết
            if (isPublic && !isOwner) {
                boolean needsUnlock = Boolean.TRUE.equals(boCauHoi.getCanMoKhoa()) 
                        && boCauHoi.getGiaMoKhoa() != null 
                        && boCauHoi.getGiaMoKhoa() > 0;
                
                if (needsUnlock) {
                    boolean hasUnlocked = hasUserUnlockedBo(id, currentUserId);
                    if (!hasUnlocked) {
                        throw new PermissionDenyException(
                                "Bộ câu hỏi này cần được mở khóa bằng " + boCauHoi.getGiaMoKhoa() + " vàng trước khi xem chi tiết"
                        );
                    }
                }
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
                                          Long creatorId,
                                          boolean isAdmin) {
        Page<BoCauHoi> page = boCauHoiRepository.searchBoCauHoi(
                pageRequest,
                keyword,
                chuDeId,
                cheDoHienThi,
                trangThai,
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
        return boCauHoiRepository.save(boCauHoi);
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

        // lưu record đã mở khóa
        BoCauHoiMoKhoa record = BoCauHoiMoKhoa.builder()
                .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                .boCauHoi(bo)
                .moKhoaLuc(Instant.now())
                .build();
        boCauHoiMoKhoaRepository.save(record);

        return UnlockBoCauHoiResponse.builder()
                .boCauHoiId(boCauHoiId)
                .giaMoKhoa(price)
                .tienVangTruoc(currentGold)
                .tienVangSau(afterGold)
                .daMoKhoaTruocDo(false)
                .build();
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

}
