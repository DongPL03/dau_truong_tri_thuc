package com.app.backend.services.community;

import com.app.backend.dtos.community.BaoCaoDTO;
import com.app.backend.dtos.community.XuLyBaoCaoDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.constant.LoaiBaoCao;
import com.app.backend.models.constant.TrangThaiBaoCao;
import com.app.backend.models.constant.TrangThaiBaiViet;
import com.app.backend.repositories.*;
import com.app.backend.responses.community.BaoCaoResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class BaoCaoService implements IBaoCaoService {

    private final IBaoCaoRepository baoCaoRepository;
    private final IBaiVietRepository baiVietRepository;
    private final IBinhLuanRepository binhLuanRepository;
    private final INguoiDungRepository nguoiDungRepository;

    @Override
    @Transactional
    public BaoCaoResponse createReport(Long userId, BaoCaoDTO dto) throws Exception {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // Validate: phải có ít nhất 1 trong 2 (bài viết hoặc bình luận)
        if (dto.getBaiVietId() == null && dto.getBinhLuanId() == null) {
            throw new IllegalArgumentException("Phải chọn bài viết hoặc bình luận để báo cáo");
        }

        BaiViet baiViet = null;
        BinhLuan binhLuan = null;

        if (dto.getBaiVietId() != null) {
            baiViet = baiVietRepository.findById(dto.getBaiVietId())
                    .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

            // Kiểm tra đã báo cáo chưa
            if (baoCaoRepository.existsByBaiViet_IdAndNguoiBaoCao_Id(dto.getBaiVietId(), userId)) {
                throw new IllegalStateException("Bạn đã báo cáo bài viết này rồi");
            }
        }

        if (dto.getBinhLuanId() != null) {
            binhLuan = binhLuanRepository.findById(dto.getBinhLuanId())
                    .orElseThrow(() -> new DataNotFoundException("Bình luận không tồn tại"));

            // Kiểm tra đã báo cáo chưa
            if (baoCaoRepository.existsByBinhLuan_IdAndNguoiBaoCao_Id(dto.getBinhLuanId(), userId)) {
                throw new IllegalStateException("Bạn đã báo cáo bình luận này rồi");
            }
        }

        LoaiBaoCao loaiBaoCao;
        try {
            loaiBaoCao = LoaiBaoCao.valueOf(dto.getLoai().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Loại báo cáo không hợp lệ");
        }

        BaoCao baoCao = BaoCao.builder()
                .baiViet(baiViet)
                .binhLuan(binhLuan)
                .nguoiBaoCao(user)
                .loaiBaoCao(loaiBaoCao)
                .chiTiet(dto.getChiTiet())
                .build();

        BaoCao saved = baoCaoRepository.save(baoCao);

        return BaoCaoResponse.fromEntity(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaoCaoResponse> getMyReports(Long userId, Pageable pageable) {
        Page<BaoCao> reports = baoCaoRepository.findByNguoiBaoCao_IdOrderByTaoLucDesc(userId, pageable);
        return reports.map(BaoCaoResponse::fromEntity);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaoCaoResponse> getAllReports(String status, Pageable pageable) {
        Page<BaoCao> reports;

        if (status != null && !status.isEmpty()) {
            try {
                TrangThaiBaoCao trangThai = TrangThaiBaoCao.valueOf(status.toUpperCase());
                reports = baoCaoRepository.findByTrangThaiOrderByTaoLucDesc(trangThai, pageable);
            } catch (IllegalArgumentException e) {
                reports = baoCaoRepository.findAllByOrderByTaoLucDesc(pageable);
            }
        } else {
            reports = baoCaoRepository.findAllByOrderByTaoLucDesc(pageable);
        }

        return reports.map(BaoCaoResponse::fromEntity);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaoCaoResponse> getPendingReports(Pageable pageable) {
        Page<BaoCao> reports = baoCaoRepository.findByTrangThaiOrderByTaoLucDesc(TrangThaiBaoCao.PENDING, pageable);
        return reports.map(BaoCaoResponse::fromEntity);
    }

    @Override
    @Transactional(readOnly = true)
    public BaoCaoResponse getReportById(Long reportId) throws Exception {
        BaoCao baoCao = baoCaoRepository.findById(reportId)
                .orElseThrow(() -> new DataNotFoundException("Báo cáo không tồn tại"));

        return BaoCaoResponse.fromEntity(baoCao);
    }

    @Override
    @Transactional
    public BaoCaoResponse resolveReport(Long adminId, Long reportId, XuLyBaoCaoDTO dto) throws Exception {
        BaoCao baoCao = baoCaoRepository.findById(reportId)
                .orElseThrow(() -> new DataNotFoundException("Báo cáo không tồn tại"));

        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        TrangThaiBaoCao trangThai;
        try {
            trangThai = TrangThaiBaoCao.valueOf(dto.trangThai().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Trạng thái không hợp lệ");
        }

        baoCao.setTrangThai(trangThai);
        baoCao.setXuLyBoi(admin);
        baoCao.setGhiChuXuLy(dto.ghiChu());
        baoCao.setXuLyLuc(Instant.now());

        // Nếu chọn ẩn nội dung
        if (Boolean.TRUE.equals(dto.anNoiDung())) {
            if (baoCao.getBaiViet() != null) {
                baoCao.getBaiViet().setTrangThai(TrangThaiBaiViet.HIDDEN);
                baiVietRepository.save(baoCao.getBaiViet());
            }
            if (baoCao.getBinhLuan() != null) {
                baoCao.getBinhLuan().setBiAn(true);
                binhLuanRepository.save(baoCao.getBinhLuan());
            }
        }

        BaoCao saved = baoCaoRepository.save(baoCao);

        return BaoCaoResponse.fromEntity(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public long countPendingReports() {
        return baoCaoRepository.countByTrangThai(TrangThaiBaoCao.PENDING);
    }

    @Override
    @Transactional
    public BaoCaoResponse dismissReport(Long adminId, Long reportId) throws Exception {
        BaoCao baoCao = baoCaoRepository.findById(reportId)
                .orElseThrow(() -> new DataNotFoundException("Báo cáo không tồn tại"));

        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        baoCao.setTrangThai(TrangThaiBaoCao.DISMISSED);
        baoCao.setXuLyBoi(admin);
        baoCao.setXuLyLuc(Instant.now());
        baoCao.setGhiChuXuLy("Đã bỏ qua báo cáo");

        BaoCao saved = baoCaoRepository.save(baoCao);
        return BaoCaoResponse.fromEntity(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public Map<String, Object> getReportStats() {
        Map<String, Object> stats = new HashMap<>();
        
        stats.put("pending", baoCaoRepository.countByTrangThai(TrangThaiBaoCao.PENDING));
        stats.put("resolved", baoCaoRepository.countByTrangThai(TrangThaiBaoCao.RESOLVED));
        stats.put("dismissed", baoCaoRepository.countByTrangThai(TrangThaiBaoCao.DISMISSED));
        stats.put("total", baoCaoRepository.count());
        
        // Thống kê theo loại
        Map<String, Long> byType = new HashMap<>();
        for (LoaiBaoCao loai : LoaiBaoCao.values()) {
            byType.put(loai.name(), baoCaoRepository.countByLoaiBaoCao(loai));
        }
        stats.put("byType", byType);
        
        return stats;
    }
}
