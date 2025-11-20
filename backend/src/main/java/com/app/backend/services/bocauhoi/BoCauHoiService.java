package com.app.backend.services.bocauhoi;

import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.models.CauHoi;
import com.app.backend.models.ChuDe;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.constant.CheDoHienThi;
import com.app.backend.models.constant.TrangThaiBoCauHoi;
import com.app.backend.repositories.*;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class BoCauHoiService implements IBoCauHoiService {
    private final IBoCauHoiRepository boCauHoiRepository;
    private final IChuDeRepository chuDeRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final IVaiTroRepository vaiTroRepository;


//    @Override
//    public BoCauHoi create(BoCauHoiDTO boCauHoiDTO, Long currentUserId) throws DataNotFoundException, PermissionDenyException {
//        ChuDe chuDe = chuDeRepository.findById(boCauHoiDTO.getChuDeId())
//                .orElseThrow(() -> new DataNotFoundException("Chủ đề không tồn tại"));
//
//        NguoiDung taoBoi = nguoiDungRepository.findById(currentUserId)
//                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
//        String RoleUser = vaiTroRepository.findById(taoBoi.getVaiTro().getId())
//                .orElseThrow(() -> new DataNotFoundException("Vai trò không tồn tại")).getTenVaiTro();
//
//        String role = taoBoi.getVaiTro().getTenVaiTro();
//        if (!List.of("user", "admin").contains(role.toLowerCase())) {
//            throw new PermissionDenyException("Bạn không có quyền tạo bộ câu hỏi");
//        }
//
//
//        String AdminRoleName = "admin";
//        String UserRoleName = "user";
//        String newTrangThai;
//        BoCauHoi boCauHoi = BoCauHoi.builder()
//                .tieuDe(boCauHoiDTO.getTieuDe())
//                .moTa(boCauHoiDTO.getMoTa())
//                .chuDe(chuDe)
//                .cheDoHienThi(boCauHoiDTO.getCheDoHienThi())
//                .taoBoi(taoBoi)
//                .trangThai(RoleUser.equals(AdminRoleName) ? TrangThaiBoCauHoi.DA_DUYET : TrangThaiBoCauHoi.CHO_DUYET)
//                .build();
//        return boCauHoiRepository.save(boCauHoi);
//    }

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

        // 4. Xây dựng đối tượng
        BoCauHoi boCauHoi = BoCauHoi.builder()
                .tieuDe(boCauHoiDTO.getTieuDe())
                .moTa(boCauHoiDTO.getMoTa())
                .chuDe(chuDe)
                .cheDoHienThi(boCauHoiDTO.getCheDoHienThi())
                .isXoa(false)
                .taoBoi(taoBoi)
                .trangThai(trangThaiMoi) // Sử dụng trạng thái đã được xác định ở trên
                .build();

        return boCauHoiRepository.save(boCauHoi);
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
            boolean isOwner = boCauHoi.getTaoBoi().getId().equals(currentUserId);
            boolean isPublic = boCauHoi.getCheDoHienThi() == CheDoHienThi.PUBLIC;
            if (!isOwner && !isPublic) {
                throw new PermissionDenyException("Bạn không có quyền xem bộ câu hỏi này");
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

//    @Override
//    @Transactional
//    public BoCauHoi markOfficial(Long id, Long adminId) {
//        BoCauHoi boCauHoi = boCauHoiRepo.findById(id)
//                .orElseThrow(() -> new IllegalArgumentException("Bộ câu hỏi không tồn tại"));
//        boCauHoi.setIsOfficial(true); // cần có field is_official trong BoCauHoi
//        return boCauHoi.save(boCauHoi);
//    }

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
}
