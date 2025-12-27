package com.app.backend.services.khoahoc;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.KhoaHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.repositories.*;
import com.app.backend.responses.khoahoc.BoCauHoiTrongKhoaResponse;
import com.app.backend.responses.khoahoc.KhoaHoiDetailResponse;
import com.app.backend.responses.khoahoc.KhoaHoiResponse;
import com.app.backend.responses.khoahoc.TienDoKhoaHoiResponse;
import com.app.backend.responses.khoahoc.UnlockKhoaHocResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class KhoaHoiService implements IKhoaHoiService {

    private final IKhoaHocRepository khoaHocRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;
    private final IChuDeRepository chuDeRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ITienDoKhoaHocRepository tienDoKhoaHocRepository;
    private final ITienDoBoCauHoiTrongKhoaRepository tienDoBoCauHoiTrongKhoaRepository;
    private final IBoCauHoiMoKhoaRepository boCauHoiMoKhoaRepository;
    private final IKhoaHocMoKhoaRepository khoaHocMoKhoaRepository;
    private final IBangXepHangRepository bangXepHangRepository;
    private final ITienDoKhoaHoiService tienDoKhoaHoiService;
    private final SecurityUtils securityUtils;

    @Override
    @Transactional
    public KhoaHoc create(KhoaHoiDTO khoaHoiDTO, Long currentUserId) throws DataNotFoundException, PermissionDenyException {
        ChuDe chuDe = chuDeRepository.findById(khoaHoiDTO.getChuDeId())
                .orElseThrow(() -> new DataNotFoundException("Chủ đề không tồn tại"));

        NguoiDung taoBoi = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // Kiểm tra quyền (chỉ admin mới được tạo khóa học)
        String role = taoBoi.getVaiTro() != null ? taoBoi.getVaiTro().getTenVaiTro().toLowerCase() : "";
        if (!"admin".equals(role)) {
            throw new PermissionDenyException("Chỉ admin mới có quyền tạo khóa học");
        }

        // Tạo khóa học
        KhoaHoc khoaHoc = KhoaHoc.builder()
                .tieuDe(khoaHoiDTO.getTieuDe())
                .moTa(khoaHoiDTO.getMoTa())
                .hinhAnh(khoaHoiDTO.getHinhAnh())
                .chuDe(chuDe)
                .taoBoi(taoBoi)
                .trangThai(khoaHoiDTO.getTrangThai() != null ? khoaHoiDTO.getTrangThai() : "DRAFT")
                .giaMoKhoa(khoaHoiDTO.getGiaMoKhoa() != null ? khoaHoiDTO.getGiaMoKhoa() : 0L)
                .thuTu(khoaHoiDTO.getThuTu() != null ? khoaHoiDTO.getThuTu() : 0)
                .isXoa(false)
                .danhSachBoCauHoi(new ArrayList<>())
                .build();

        khoaHoc = khoaHocRepository.save(khoaHoc);

        // Thêm danh sách bộ câu hỏi nếu có
        if (khoaHoiDTO.getDanhSachBoCauHoi() != null && !khoaHoiDTO.getDanhSachBoCauHoi().isEmpty()) {
            List<KhoaHocBoCauHoi> danhSachBoCauHoi = new ArrayList<>();
            for (KhoaHoiDTO.BoCauHoiTrongKhoaDTO boCauHoiDTO : khoaHoiDTO.getDanhSachBoCauHoi()) {
                BoCauHoi boCauHoi = boCauHoiRepository.findById(boCauHoiDTO.getBoCauHoiId())
                        .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại: " + boCauHoiDTO.getBoCauHoiId()));

                KhoaHocBoCauHoi khoaHocBoCauHoi = KhoaHocBoCauHoi.builder()
                        .khoaHoc(khoaHoc)
                        .boCauHoi(boCauHoi)
                        .thuTu(boCauHoiDTO.getThuTu())
                        .isBatBuoc(boCauHoiDTO.getIsBatBuoc() != null ? boCauHoiDTO.getIsBatBuoc() : true)
                        .diemToiThieu(boCauHoiDTO.getDiemToiThieu() != null ? boCauHoiDTO.getDiemToiThieu() : 0)
                        .build();

                danhSachBoCauHoi.add(khoaHocBoCauHoi);
            }
            khoaHocBoCauHoiRepository.saveAll(danhSachBoCauHoi);
            khoaHoc.setDanhSachBoCauHoi(danhSachBoCauHoi);
        }

        return khoaHoc;
    }

    @Override
    @Transactional
    public KhoaHoc update(Long id, KhoaHoiDTO khoaHoiDTO, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        KhoaHoc khoaHoc = khoaHocRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        if (!isAdmin && !khoaHoc.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được sửa khóa học của mình");
        }

        // Cập nhật thông tin cơ bản
        if (khoaHoiDTO.getTieuDe() != null) khoaHoc.setTieuDe(khoaHoiDTO.getTieuDe());
        if (khoaHoiDTO.getMoTa() != null) khoaHoc.setMoTa(khoaHoiDTO.getMoTa());
        if (khoaHoiDTO.getHinhAnh() != null) khoaHoc.setHinhAnh(khoaHoiDTO.getHinhAnh());
        if (khoaHoiDTO.getTrangThai() != null) khoaHoc.setTrangThai(khoaHoiDTO.getTrangThai());
        if (khoaHoiDTO.getGiaMoKhoa() != null) khoaHoc.setGiaMoKhoa(khoaHoiDTO.getGiaMoKhoa());
        if (khoaHoiDTO.getThuTu() != null) khoaHoc.setThuTu(khoaHoiDTO.getThuTu());

        if (khoaHoiDTO.getChuDeId() != null) {
            ChuDe chuDe = chuDeRepository.findById(khoaHoiDTO.getChuDeId())
                    .orElseThrow(() -> new DataNotFoundException("Chủ đề không tồn tại"));
            khoaHoc.setChuDe(chuDe);
        }

        // Cập nhật danh sách bộ câu hỏi nếu có
        if (khoaHoiDTO.getDanhSachBoCauHoi() != null) {
            // Xóa danh sách cũ
            khoaHocBoCauHoiRepository.deleteByKhoaHocId(id);

            // Thêm danh sách mới
            List<KhoaHocBoCauHoi> danhSachBoCauHoi = new ArrayList<>();
            for (KhoaHoiDTO.BoCauHoiTrongKhoaDTO boCauHoiDTO : khoaHoiDTO.getDanhSachBoCauHoi()) {
                BoCauHoi boCauHoi = boCauHoiRepository.findById(boCauHoiDTO.getBoCauHoiId())
                        .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại: " + boCauHoiDTO.getBoCauHoiId()));

                KhoaHocBoCauHoi khoaHocBoCauHoi = KhoaHocBoCauHoi.builder()
                        .khoaHoc(khoaHoc)
                        .boCauHoi(boCauHoi)
                        .thuTu(boCauHoiDTO.getThuTu())
                        .isBatBuoc(boCauHoiDTO.getIsBatBuoc() != null ? boCauHoiDTO.getIsBatBuoc() : true)
                        .diemToiThieu(boCauHoiDTO.getDiemToiThieu() != null ? boCauHoiDTO.getDiemToiThieu() : 0)
                        .build();

                danhSachBoCauHoi.add(khoaHocBoCauHoi);
            }
            khoaHocBoCauHoiRepository.saveAll(danhSachBoCauHoi);
            khoaHoc.setDanhSachBoCauHoi(danhSachBoCauHoi);
        }

        return khoaHocRepository.save(khoaHoc);
    }

    @Override
    @Transactional
    public void delete(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        KhoaHoc khoaHoc = khoaHocRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        if (!isAdmin && !khoaHoc.getTaoBoi().getId().equals(currentUserId)) {
            throw new PermissionDenyException("Bạn chỉ được xóa khóa học của mình");
        }

        khoaHocRepository.delete(khoaHoc);
    }

    @Override
    public KhoaHoc getById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        KhoaHoc khoaHoc = khoaHocRepository.findByIdAndIsXoaFalse(id)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        // Kiểm tra quyền xem
        if (!isAdmin) {
            boolean isOwner = khoaHoc.getTaoBoi() != null && khoaHoc.getTaoBoi().getId().equals(currentUserId);
            boolean isPublished = "PUBLISHED".equals(khoaHoc.getTrangThai());

            if (!isOwner && !isPublished) {
                throw new PermissionDenyException("Bạn không có quyền xem khóa học này");
            }
        }

        return khoaHoc;
    }

    @Override
    @Transactional(readOnly = true)
    public KhoaHoiDetailResponse getDetailById(Long id, Long currentUserId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        KhoaHoc khoaHoc = getById(id, currentUserId, isAdmin);

        // Lấy danh sách bộ câu hỏi
        List<KhoaHocBoCauHoi> danhSachBoCauHoi = khoaHocBoCauHoiRepository.findByKhoaHocIdOrderByThuTuAsc(id);

        // Lấy tiến độ khóa học nếu user đã đăng nhập
        TienDoKhoaHoc tienDoKhoaHoc = null;
        if (currentUserId != null) {
            tienDoKhoaHoc = tienDoKhoaHocRepository.findByNguoiDungIdAndKhoaHocId(currentUserId, id)
                    .orElse(null);
        }

        // Map sang response
        List<BoCauHoiTrongKhoaResponse> boCauHoiResponses = new ArrayList<>();
        for (KhoaHocBoCauHoi khbch : danhSachBoCauHoi) {
            // Kiểm tra unlock status
            boolean daMoKhoa = false;
            String trangThai = "CHUA_MO_KHOA";

            if (currentUserId != null) {
                boolean isOwner = khoaHoc.getTaoBoi() != null && khoaHoc.getTaoBoi().getId().equals(currentUserId);

                if (isOwner || isAdmin) {
                    daMoKhoa = true;
                    trangThai = "DA_MO_KHOA";
                } else {
                    // Force load boCauHoi nếu chưa được load
                    BoCauHoi boCauHoi = khbch.getBoCauHoi();
                    Long boCauHoiId = boCauHoi != null ? boCauHoi.getId() : null;

                    if (boCauHoiId != null) {
                        // Kiểm tra unlock status - dùng findBy thay vì existsBy để tránh cache issue
                        boolean exists = boCauHoiMoKhoaRepository
                                .findByNguoiDung_IdAndBoCauHoi_Id(currentUserId, boCauHoiId)
                                .isPresent();
                        daMoKhoa = exists;
                        
                        // Lấy progress status từ TienDoBoCauHoiTrongKhoa nếu có
                        if (tienDoKhoaHoc != null) {
                            var tienDoOptional = tienDoBoCauHoiTrongKhoaRepository
                                    .findByTienDoKhoaHocIdAndBoCauHoiId(tienDoKhoaHoc.getId(), boCauHoiId);

                            if (tienDoOptional.isPresent()) {
                                var tienDoBoCauHoi = tienDoOptional.get();
                                String progressStatus = tienDoBoCauHoi.getTrangThai();

                                if ("HOAN_THANH".equals(progressStatus)) {
                                    trangThai = "HOAN_THANH";
                                } else if ("DANG_HOC".equals(progressStatus)) {
                                    trangThai = "DANG_HOC";
                                } else {
                                    trangThai = "DA_MO_KHOA";
                                }
                            } else {
                                trangThai = daMoKhoa ? "DA_MO_KHOA" : "CHUA_MO_KHOA";
                            }
                        } else {
                            trangThai = daMoKhoa ? "DA_MO_KHOA" : "CHUA_MO_KHOA";
                        }
                    }
                }
            }

            boCauHoiResponses.add(BoCauHoiTrongKhoaResponse.from(khbch, trangThai, daMoKhoa));
        }

        // Lấy tiến độ nếu user đã đăng nhập
        TienDoKhoaHoiResponse tienDo = null;
        if (currentUserId != null) {
            Optional<TienDoKhoaHoc> tienDoOptional = tienDoKhoaHocRepository.findByNguoiDungIdAndKhoaHocId(currentUserId, id);
            if (tienDoOptional.isPresent()) {
                TienDoKhoaHoc td = tienDoOptional.get();
                System.out.println("DEBUG KhoaHoiService.getDetailById: Tìm thấy TienDoKhoaHoc - " 
                    + "soBoDaHoanThanh=" + td.getSoBoDaHoanThanh() 
                    + ", tongSoBo=" + td.getTongSoBo() 
                    + ", phanTramHoanThanh=" + td.getPhanTramHoanThanh());
                tienDo = TienDoKhoaHoiResponse.from(td);
            } else {
                System.out.println("DEBUG KhoaHoiService.getDetailById: Không tìm thấy TienDoKhoaHoc cho userId=" 
                    + currentUserId + ", khoaHocId=" + id);
            }
        }

        return KhoaHoiDetailResponse.builder()
                .khoaHoc(KhoaHoiResponse.from(khoaHoc))
                .danhSachBoCauHoi(boCauHoiResponses)
                .tienDo(tienDo)
                .build();
    }

    @Override
    public Page<KhoaHoc> findAll(PageRequest pageRequest,
                                 String keyword,
                                 Long chuDeId,
                                 String trangThai,
                                 Long creatorId,
                                 boolean isAdmin) {
        return khoaHocRepository.searchKhoaHoc(pageRequest, keyword, chuDeId, trangThai, creatorId, isAdmin);
    }

    @Override
    @Transactional
    public void softDelete(Long id, Long userId, boolean isAdmin) throws DataNotFoundException, PermissionDenyException {
        KhoaHoc khoaHoc = khoaHocRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        if (!isAdmin && !khoaHoc.getTaoBoi().getId().equals(userId)) {
            throw new PermissionDenyException("Bạn chỉ được xóa khóa học của mình");
        }

        khoaHoc.setIsXoa(true);
        khoaHocRepository.save(khoaHoc);
    }

    /**
     * Unlock khóa học bằng vàng
     * Sau khi unlock khóa học, tự động unlock bộ câu hỏi đầu tiên
     */
    @Transactional
    public UnlockKhoaHocResponse unlockKhoaHoc(Long khoaHocId, Long userId) throws Exception {
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        // Admin bypass: không cần trừ vàng, return luôn
        if (securityUtils.isAdmin()) {
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));
            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
            
            // Tạo record unlock nếu chưa có
            if (!khoaHocMoKhoaRepository.existsByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId)) {
                KhoaHocMoKhoa record = KhoaHocMoKhoa.builder()
                        .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                        .khoaHoc(khoaHoc)
                        .moKhoaLuc(Instant.now())
                        .build();
                khoaHocMoKhoaRepository.save(record);
                // Tự động unlock bộ câu hỏi đầu tiên
                unlockFirstBoCauHoiInKhoaHoc(khoaHocId, userId);
            }
            
            return UnlockKhoaHocResponse.builder()
                    .khoaHocId(khoaHocId)
                    .giaMoKhoa(khoaHoc.getGiaMoKhoa() != null ? khoaHoc.getGiaMoKhoa() : 0L)
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(khoaHocMoKhoaRepository.existsByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId))
                    .build();
        }

        // Chủ khóa học bypass: không cần trừ vàng, return luôn
        if (khoaHoc.getTaoBoi() != null && khoaHoc.getTaoBoi().getId().equals(userId)) {
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));
            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
            
            // Tạo record unlock nếu chưa có
            if (!khoaHocMoKhoaRepository.existsByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId)) {
                KhoaHocMoKhoa record = KhoaHocMoKhoa.builder()
                        .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                        .khoaHoc(khoaHoc)
                        .moKhoaLuc(Instant.now())
                        .build();
                khoaHocMoKhoaRepository.save(record);
                // Tự động unlock bộ câu hỏi đầu tiên
                unlockFirstBoCauHoiInKhoaHoc(khoaHocId, userId);
            }
            
            return UnlockKhoaHocResponse.builder()
                    .khoaHocId(khoaHocId)
                    .giaMoKhoa(khoaHoc.getGiaMoKhoa() != null ? khoaHoc.getGiaMoKhoa() : 0L)
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(true)
                    .build();
        }

        // Kiểm tra giá mở khóa
        if (khoaHoc.getGiaMoKhoa() == null || khoaHoc.getGiaMoKhoa() <= 0) {
            throw new IllegalStateException("Khóa học này không cần mở khóa bằng vàng");
        }

        // Kiểm tra đã mở trước đó chưa
        Optional<KhoaHocMoKhoa> existed = khoaHocMoKhoaRepository
                .findByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId);
        if (existed.isPresent()) {
            // Không trừ vàng nữa, chỉ trả về info
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));
            long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;

            return UnlockKhoaHocResponse.builder()
                    .khoaHocId(khoaHocId)
                    .giaMoKhoa(khoaHoc.getGiaMoKhoa())
                    .tienVangTruoc(tienVang)
                    .tienVangSau(tienVang)
                    .daMoKhoaTruocDo(true)
                    .build();
        }

        // Lấy BXH để kiểm tra vàng
        BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy bảng xếp hạng của người dùng"));

        long currentGold = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;
        long price = khoaHoc.getGiaMoKhoa();

        if (currentGold < price) {
            throw new IllegalStateException("Không đủ vàng để mở khóa khóa học này");
        }

        // Trừ vàng
        long afterGold = currentGold - price;
        bxh.setTienVang(afterGold);
        bangXepHangRepository.save(bxh);

        // Lưu record đã mở khóa
        KhoaHocMoKhoa record = KhoaHocMoKhoa.builder()
                .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                .khoaHoc(khoaHoc)
                .moKhoaLuc(Instant.now())
                .build();
        khoaHocMoKhoaRepository.save(record);
        khoaHocMoKhoaRepository.flush(); // Đảm bảo data được ghi vào DB ngay

        // Tự động unlock bộ câu hỏi đầu tiên trong khóa học
        unlockFirstBoCauHoiInKhoaHoc(khoaHocId, userId);

        return UnlockKhoaHocResponse.builder()
                .khoaHocId(khoaHocId)
                .giaMoKhoa(price)
                .tienVangTruoc(currentGold)
                .tienVangSau(afterGold)
                .daMoKhoaTruocDo(false)
                .build();
    }

    /**
     * Tự động unlock bộ câu hỏi đầu tiên trong khóa học (miễn phí)
     */
    private void unlockFirstBoCauHoiInKhoaHoc(Long khoaHocId, Long userId) {
        // Lấy bộ câu hỏi đầu tiên (thuTu = 1 hoặc nhỏ nhất)
        List<KhoaHocBoCauHoi> danhSachBo = khoaHocBoCauHoiRepository
                .findByKhoaHocIdOrderByThuTuAsc(khoaHocId);
        
        if (danhSachBo.isEmpty()) {
            return; // Không có bộ câu hỏi nào
        }

        KhoaHocBoCauHoi firstBo = danhSachBo.get(0);
        BoCauHoi boCauHoi = firstBo.getBoCauHoi();
        
        // Kiểm tra xem đã unlock chưa
        boolean daMoKhoa = boCauHoiMoKhoaRepository
                .existsByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoi.getId());
        
        if (!daMoKhoa) {
            // Tự động unlock (miễn phí)
            BoCauHoiMoKhoa unlock = BoCauHoiMoKhoa.builder()
                    .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                    .boCauHoi(boCauHoi)
                    .moKhoaLuc(Instant.now())
                    .build();
            boCauHoiMoKhoaRepository.save(unlock);
            boCauHoiMoKhoaRepository.flush();
        }
    }

    /**
     * Kiểm tra user đã unlock khóa học chưa
     */
    public boolean hasUserUnlockedKhoaHoc(Long khoaHocId, Long userId) {
        if (userId == null) {
            return false;
        }
        // Admin bypass unlock
        if (securityUtils.isAdmin()) {
            return true;
        }
        // Chủ khóa học bypass unlock
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId).orElse(null);
        if (khoaHoc != null && khoaHoc.getTaoBoi() != null && khoaHoc.getTaoBoi().getId().equals(userId)) {
            return true;
        }
        // User thường: check record unlock
        return khoaHocMoKhoaRepository.existsByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId);
    }
}

