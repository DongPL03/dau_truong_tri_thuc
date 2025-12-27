package com.app.backend.services.khoahoc;

import com.app.backend.dtos.AddBoCauHoiToKhoaHocDTO;
import com.app.backend.dtos.UpdateBoCauHoiInKhoaHocDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.models.KhoaHoc;
import com.app.backend.models.KhoaHocBoCauHoi;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.IKhoaHocBoCauHoiRepository;
import com.app.backend.repositories.IKhoaHocRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class KhoaHocBoCauHoiService implements IKhoaHocBoCauHoiService {

    private final IKhoaHocRepository khoaHocRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;

    @Override
    @Transactional
    public KhoaHocBoCauHoi addBoCauHoiToKhoaHoc(Long khoaHocId, AddBoCauHoiToKhoaHocDTO dto, Long adminId)
            throws DataNotFoundException, PermissionDenyException {
        // Kiểm tra khóa học tồn tại và admin có quyền
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        // Kiểm tra quyền admin
        if (!khoaHoc.getTaoBoi().getId().equals(adminId)) {
            throw new PermissionDenyException("Chỉ admin tạo khóa học mới có quyền quản lý bộ câu hỏi");
        }

        // Kiểm tra bộ câu hỏi tồn tại
        BoCauHoi boCauHoi = boCauHoiRepository.findById(dto.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // Kiểm tra bộ câu hỏi đã được duyệt
        if (!"DA_DUYET".equals(boCauHoi.getTrangThai())) {
            throw new IllegalStateException("Chỉ có thể thêm bộ câu hỏi đã được duyệt vào khóa học");
        }

        // Kiểm tra bộ câu hỏi phải do admin tạo
        NguoiDung creator = boCauHoi.getTaoBoi();
        if (creator == null || creator.getVaiTro() == null || 
            !"admin".equalsIgnoreCase(creator.getVaiTro().getTenVaiTro())) {
            throw new IllegalStateException("Chỉ có thể thêm bộ câu hỏi do admin tạo vào khóa học. Vui lòng duplicate bộ câu hỏi của user trước.");
        }

        // Kiểm tra loại sử dụng: chỉ cho phép COURSE_ONLY
        String loaiSuDung = boCauHoi.getLoaiSuDung();
        if (loaiSuDung == null || !loaiSuDung.equals("COURSE_ONLY")) {
            throw new IllegalStateException("Chỉ có thể thêm bộ câu hỏi có loại sử dụng là COURSE_ONLY vào khóa học");
        }

        // Kiểm tra bộ câu hỏi đã có trong khóa học chưa
        Optional<KhoaHocBoCauHoi> existing = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndBoCauHoiId(khoaHocId, dto.getBoCauHoiId());
        if (existing.isPresent()) {
            throw new IllegalStateException("Bộ câu hỏi đã có trong khóa học này");
        }

        // Kiểm tra thứ tự đã tồn tại chưa
        Optional<KhoaHocBoCauHoi> existingOrder = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndThuTu(khoaHocId, dto.getThuTu());
        if (existingOrder.isPresent()) {
            // Tự động tăng thứ tự cho các bộ câu hỏi sau
            List<KhoaHocBoCauHoi> laterItems = khoaHocBoCauHoiRepository
                    .findByKhoaHocIdOrderByThuTuAsc(khoaHocId)
                    .stream()
                    .filter(item -> item.getThuTu() >= dto.getThuTu())
                    .toList();
            for (KhoaHocBoCauHoi item : laterItems) {
                item.setThuTu(item.getThuTu() + 1);
                khoaHocBoCauHoiRepository.save(item);
            }
        }

        // Tạo mới
        KhoaHocBoCauHoi khoaHocBoCauHoi = KhoaHocBoCauHoi.builder()
                .khoaHoc(khoaHoc)
                .boCauHoi(boCauHoi)
                .thuTu(dto.getThuTu())
                .isBatBuoc(dto.getIsBatBuoc() != null ? dto.getIsBatBuoc() : true)
                .diemToiThieu(dto.getDiemToiThieu() != null ? dto.getDiemToiThieu() : 0)
                .build();

        return khoaHocBoCauHoiRepository.save(khoaHocBoCauHoi);
    }

    @Override
    @Transactional
    public KhoaHocBoCauHoi updateBoCauHoiInKhoaHoc(Long khoaHocId, Long boCauHoiId, UpdateBoCauHoiInKhoaHocDTO dto, Long adminId)
            throws DataNotFoundException, PermissionDenyException {
        // Kiểm tra khóa học tồn tại và admin có quyền
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        if (!khoaHoc.getTaoBoi().getId().equals(adminId)) {
            throw new PermissionDenyException("Chỉ admin tạo khóa học mới có quyền quản lý bộ câu hỏi");
        }

        // Tìm bộ câu hỏi trong khóa học
        KhoaHocBoCauHoi khoaHocBoCauHoi = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndBoCauHoiId(khoaHocId, boCauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không có trong khóa học này"));

        // Cập nhật các trường
        if (dto.getThuTu() != null) {
            // Kiểm tra thứ tự mới có trùng không
            Optional<KhoaHocBoCauHoi> existingOrder = khoaHocBoCauHoiRepository
                    .findByKhoaHocIdAndThuTu(khoaHocId, dto.getThuTu());
            if (existingOrder.isPresent() && !existingOrder.get().getId().equals(khoaHocBoCauHoi.getId())) {
                // Đổi chỗ thứ tự
                int oldOrder = khoaHocBoCauHoi.getThuTu();
                int newOrder = dto.getThuTu();
                existingOrder.get().setThuTu(oldOrder);
                khoaHocBoCauHoiRepository.save(existingOrder.get());
            }
            khoaHocBoCauHoi.setThuTu(dto.getThuTu());
        }

        if (dto.getIsBatBuoc() != null) {
            khoaHocBoCauHoi.setIsBatBuoc(dto.getIsBatBuoc());
        }

        if (dto.getDiemToiThieu() != null) {
            khoaHocBoCauHoi.setDiemToiThieu(dto.getDiemToiThieu());
        }

        return khoaHocBoCauHoiRepository.save(khoaHocBoCauHoi);
    }

    @Override
    @Transactional
    public void removeBoCauHoiFromKhoaHoc(Long khoaHocId, Long boCauHoiId, Long adminId)
            throws DataNotFoundException, PermissionDenyException {
        // Kiểm tra khóa học tồn tại và admin có quyền
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        if (!khoaHoc.getTaoBoi().getId().equals(adminId)) {
            throw new PermissionDenyException("Chỉ admin tạo khóa học mới có quyền quản lý bộ câu hỏi");
        }

        // Tìm và xóa
        KhoaHocBoCauHoi khoaHocBoCauHoi = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndBoCauHoiId(khoaHocId, boCauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không có trong khóa học này"));

        int removedOrder = khoaHocBoCauHoi.getThuTu();
        khoaHocBoCauHoiRepository.delete(khoaHocBoCauHoi);

        // Cập nhật thứ tự cho các bộ câu hỏi sau
        List<KhoaHocBoCauHoi> laterItems = khoaHocBoCauHoiRepository
                .findByKhoaHocIdOrderByThuTuAsc(khoaHocId)
                .stream()
                .filter(item -> item.getThuTu() > removedOrder)
                .toList();
        for (KhoaHocBoCauHoi item : laterItems) {
            item.setThuTu(item.getThuTu() - 1);
            khoaHocBoCauHoiRepository.save(item);
        }
    }
}

