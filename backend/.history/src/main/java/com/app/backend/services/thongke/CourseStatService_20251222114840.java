package com.app.backend.services.thongke;

import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TienDoKhoaHoc;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.IKhoaHocBoCauHoiRepository;
import com.app.backend.repositories.IPhienLuyenTapRepository;
import com.app.backend.repositories.ITienDoBoCauHoiTrongKhoaRepository;
import com.app.backend.repositories.ITienDoKhoaHocRepository;
import com.app.backend.responses.thongke.CourseStatResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CourseStatService implements ICourseStatService {

    private final ITienDoKhoaHocRepository tienDoKhoaHocRepository;
    private final ITienDoBoCauHoiTrongKhoaRepository tienDoBoCauHoiTrongKhoaRepository;
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;

    @Override
    @Transactional(readOnly = true)
    public CourseStatResponse getCourseStatsForUser(Long userId) {
        // 1. Lấy tất cả tiến độ khóa học của user
        List<TienDoKhoaHoc> tienDoKhoaHocList = tienDoKhoaHocRepository.findByNguoiDungIdOrderByCapNhatLucDesc(userId);
        
        int tongSoKhoaHoc = tienDoKhoaHocList.size();
        int soKhoaHocDaHoanThanh = 0;
        int soKhoaHocDangHoc = 0;
        int tongSoBoCauHoiDaHoanThanh = 0;
        BigDecimal tongPhanTramHoanThanh = BigDecimal.ZERO;

        // 2. Đếm số khóa học đã hoàn thành và đang học
        for (TienDoKhoaHoc tienDo : tienDoKhoaHocList) {
            if ("HOAN_THANH".equals(tienDo.getTrangThai())) {
                soKhoaHocDaHoanThanh++;
            } else if ("DANG_HOC".equals(tienDo.getTrangThai())) {
                soKhoaHocDangHoc++;
            }
            
            // Đếm số bộ câu hỏi đã hoàn thành
            List<com.app.backend.models.TienDoBoCauHoiTrongKhoa> completedBoCauHoi = 
                tienDoBoCauHoiTrongKhoaRepository.findCompletedBoCauHoi(tienDo.getId());
            tongSoBoCauHoiDaHoanThanh += completedBoCauHoi.size();
            
            // Tính tổng phần trăm hoàn thành
            if (tienDo.getPhanTramHoanThanh() != null) {
                tongPhanTramHoanThanh = tongPhanTramHoanThanh.add(tienDo.getPhanTramHoanThanh());
            }
        }

        // 3. Tính phần trăm hoàn thành trung bình
        BigDecimal phanTramHoanThanhTrungBinh = tongSoKhoaHoc > 0
                ? tongPhanTramHoanThanh.divide(BigDecimal.valueOf(tongSoKhoaHoc), 2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        // 4. Lấy thống kê từ các phiên luyện tập trong courses
        // Query trực tiếp để lấy các phiên luyện tập thuộc courses (hiệu quả hơn)
        List<PhienLuyenTap> phienTrongKhoaHoc = phienLuyenTapRepository.findByNguoiDungIdAndBelongsToCourse(userId);

        long tongSoPhienLuyenTap = phienTrongKhoaHoc.size();
        
        // Tính tổng số bộ câu hỏi đã làm (distinct)
        long tongSoBoCauHoiDaLam = phienTrongKhoaHoc.stream()
                .map(phien -> phien.getBoCauHoi() != null ? phien.getBoCauHoi().getId() : null)
                .filter(id -> id != null)
                .distinct()
                .count();

        // Tính điểm trung bình và độ chính xác trung bình
        BigDecimal diemTrungBinh = BigDecimal.ZERO;
        BigDecimal doChinhXacTrungBinh = BigDecimal.ZERO;
        long tongThoiGianLuyenTapMs = 0;

        if (!phienTrongKhoaHoc.isEmpty()) {
            BigDecimal tongDiem = BigDecimal.ZERO;
            BigDecimal tongDoChinhXac = BigDecimal.ZERO;
            
            for (PhienLuyenTap phien : phienTrongKhoaHoc) {
                if (phien.getDiemSo() != null) {
                    tongDiem = tongDiem.add(BigDecimal.valueOf(phien.getDiemSo()));
                }
                if (phien.getDoChinhXac() != null) {
                    tongDoChinhXac = tongDoChinhXac.add(phien.getDoChinhXac());
                }
                if (phien.getThoiGianTbMs() != null && phien.getTongCauHoi() != null) {
                    tongThoiGianLuyenTapMs += phien.getThoiGianTbMs() * phien.getTongCauHoi();
                }
            }
            
            diemTrungBinh = tongDiem.divide(BigDecimal.valueOf(tongSoPhienLuyenTap), 2, RoundingMode.HALF_UP);
            doChinhXacTrungBinh = tongDoChinhXac.divide(BigDecimal.valueOf(tongSoPhienLuyenTap), 2, RoundingMode.HALF_UP);
        }

        return CourseStatResponse.builder()
                .tongSoKhoaHoc(tongSoKhoaHoc)
                .soKhoaHocDaHoanThanh(soKhoaHocDaHoanThanh)
                .soKhoaHocDangHoc(soKhoaHocDangHoc)
                .tongSoBoCauHoiDaLam((int) tongSoBoCauHoiDaLam)
                .tongSoBoCauHoiDaHoanThanh(tongSoBoCauHoiDaHoanThanh)
                .diemTrungBinh(diemTrungBinh)
                .doChinhXacTrungBinh(doChinhXacTrungBinh)
                .tongSoPhienLuyenTap(tongSoPhienLuyenTap)
                .tongThoiGianLuyenTapMs(tongThoiGianLuyenTapMs)
                .phanTramHoanThanhTrungBinh(phanTramHoanThanhTrungBinh)
                .build();
    }
}

