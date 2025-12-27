package com.app.backend.services.khoahoc;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.repositories.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TienDoKhoaHoiService implements ITienDoKhoaHoiService {

    private final ITienDoKhoaHocRepository tienDoKhoaHocRepository;
    private final ITienDoBoCauHoiTrongKhoaRepository tienDoBoCauHoiTrongKhoaRepository;
    private final IKhoaHocRepository khoaHocRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IBoCauHoiMoKhoaRepository boCauHoiMoKhoaRepository;
    private final IKhoaHocMoKhoaRepository khoaHocMoKhoaRepository;

    @Override
    @Transactional
    public TienDoKhoaHoc getOrCreateProgress(Long userId, Long khoaHocId) throws DataNotFoundException {
        return tienDoKhoaHocRepository.findByNguoiDungIdAndKhoaHocId(userId, khoaHocId)
                .orElseGet(() -> {
                    NguoiDung nguoiDung = null;
                    try {
                        nguoiDung = nguoiDungRepository.findById(userId)
                                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
                    } catch (DataNotFoundException e) {
                        throw new RuntimeException(e);
                    }
                    KhoaHoc khoaHoc = null;
                    try {
                        khoaHoc = khoaHocRepository.findById(khoaHocId)
                                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));
                    } catch (DataNotFoundException e) {
                        throw new RuntimeException(e);
                    }

                    // Đếm tổng số bộ câu hỏi trong khóa học
                    List<KhoaHocBoCauHoi> danhSachBo = khoaHocBoCauHoiRepository
                            .findByKhoaHocIdOrderByThuTuAsc(khoaHocId);
                    int tongSoBo = danhSachBo.size();

                    TienDoKhoaHoc tienDo = TienDoKhoaHoc.builder()
                            .nguoiDung(nguoiDung)
                            .khoaHoc(khoaHoc)
                            .soBoDaHoanThanh(0)
                            .tongSoBo(tongSoBo)
                            .phanTramHoanThanh(BigDecimal.ZERO)
                            .trangThai("CHUA_BAT_DAU")
                            .ngayBatDau(null)
                            .ngayHoanThanh(null)
                            .build();

                    return tienDoKhoaHocRepository.save(tienDo);
                });
    }

    @Override
    @Transactional(propagation = org.springframework.transaction.annotation.Propagation.REQUIRES_NEW)
    public void updateProgressAfterPractice(Long userId, Long boCauHoiId, Integer diemSo, BigDecimal doChinhXac) throws DataNotFoundException {
        System.out.println("DEBUG TienDoKhoaHoiService.updateProgressAfterPractice: userId=" + userId 
            + ", boCauHoiId=" + boCauHoiId + ", diemSo=" + diemSo);
        
        // Tìm khóa học chứa bộ câu hỏi này
        KhoaHocBoCauHoi khbch = khoaHocBoCauHoiRepository.findByBoCauHoiId(boCauHoiId)
                .orElse(null);

        if (khbch == null) {
            // Bộ câu hỏi không thuộc khóa học nào, không cần cập nhật progress
            System.out.println("DEBUG TienDoKhoaHoiService: Bộ câu hỏi không thuộc khóa học nào");
            return;
        }

        Long khoaHocId = khbch.getKhoaHoc().getId();
        System.out.println("DEBUG TienDoKhoaHoiService: Tìm thấy khóa học ID = " + khoaHocId);

        // Lấy hoặc tạo progress của khóa học
        TienDoKhoaHoc tienDoKhoaHoc = getOrCreateProgress(userId, khoaHocId);

        // Cập nhật trạng thái khóa học nếu chưa bắt đầu
        if ("CHUA_BAT_DAU".equals(tienDoKhoaHoc.getTrangThai())) {
            tienDoKhoaHoc.setTrangThai("DANG_HOC");
            tienDoKhoaHoc.setNgayBatDau(Instant.now());
            tienDoKhoaHoc.setBoCauHoiHienTai(boCauHoiRepository.findById(boCauHoiId)
                    .orElse(null));
        }

        // Lấy hoặc tạo progress của bộ câu hỏi
        TienDoBoCauHoiTrongKhoa tienDoBoCauHoi = getOrCreateBoCauHoiProgress(tienDoKhoaHoc.getId(), boCauHoiId);
        System.out.println("DEBUG TienDoKhoaHoiService: Sau khi lấy tienDoBoCauHoi - trangThai=" 
            + tienDoBoCauHoi.getTrangThai() 
            + ", diemCaoNhat=" + tienDoBoCauHoi.getDiemCaoNhat()
            + ", soLanLuyenTap=" + tienDoBoCauHoi.getSoLanLuyenTap());

        // Cập nhật điểm cao nhất
        if (diemSo > tienDoBoCauHoi.getDiemCaoNhat()) {
            tienDoBoCauHoi.setDiemCaoNhat(diemSo);
            System.out.println("DEBUG TienDoKhoaHoiService: Cập nhật điểm cao nhất từ " 
                + tienDoBoCauHoi.getDiemCaoNhat() + " lên " + diemSo);
        }

        // Tăng số lần luyện tập
        tienDoBoCauHoi.setSoLanLuyenTap(tienDoBoCauHoi.getSoLanLuyenTap() + 1);
        tienDoBoCauHoi.setLanCuoiLuyenTap(Instant.now());

        // Kiểm tra xem có đạt điểm tối thiểu không
        // diemToiThieu là điểm trên thang 100 (%), doChinhXac cũng là phần trăm
        Integer diemToiThieu = khbch.getDiemToiThieu();
        boolean datDiemToiThieu = diemToiThieu == null || 
            (doChinhXac != null && doChinhXac.compareTo(BigDecimal.valueOf(diemToiThieu)) >= 0);
        System.out.println("DEBUG TienDoKhoaHoiService: diemToiThieu=" + diemToiThieu 
            + ", diemSo=" + diemSo 
            + ", doChinhXac=" + doChinhXac
            + ", datDiemToiThieu=" + datDiemToiThieu
            + ", trangThaiHienTai=" + tienDoBoCauHoi.getTrangThai());

        // Cập nhật trạng thái bộ câu hỏi
        boolean wasHoanThanh = "HOAN_THANH".equals(tienDoBoCauHoi.getTrangThai());
        
        if (datDiemToiThieu) {
            // Đạt điểm tối thiểu - đánh dấu HOAN_THANH nếu chưa
            if (!wasHoanThanh) {
                System.out.println("DEBUG TienDoKhoaHoiService: Đạt điểm tối thiểu, đánh dấu HOAN_THANH");
                tienDoBoCauHoi.setTrangThai("HOAN_THANH");
            } else {
                System.out.println("DEBUG TienDoKhoaHoiService: Đạt điểm tối thiểu, bộ câu hỏi đã HOAN_THANH từ trước");
            }
            
            // Save trước để query đếm được bộ này
            tienDoBoCauHoiTrongKhoaRepository.save(tienDoBoCauHoi);
            tienDoBoCauHoiTrongKhoaRepository.flush();

            // Luôn đếm lại và cập nhật số bộ đã hoàn thành (để đảm bảo đồng bộ)
            List<TienDoBoCauHoiTrongKhoa> completedList = tienDoBoCauHoiTrongKhoaRepository
                    .findCompletedBoCauHoi(tienDoKhoaHoc.getId());
            long soBoHoanThanh = completedList.size();
            System.out.println("DEBUG TienDoKhoaHoiService: Số bộ đã hoàn thành = " + soBoHoanThanh 
                + ", Tổng số bộ = " + tienDoKhoaHoc.getTongSoBo());
            tienDoKhoaHoc.setSoBoDaHoanThanh((int) soBoHoanThanh);

            // Tính phần trăm hoàn thành
            if (tienDoKhoaHoc.getTongSoBo() > 0) {
                BigDecimal phanTram = BigDecimal.valueOf(tienDoKhoaHoc.getSoBoDaHoanThanh() * 100.0 / tienDoKhoaHoc.getTongSoBo())
                        .setScale(2, RoundingMode.HALF_UP);
                tienDoKhoaHoc.setPhanTramHoanThanh(phanTram);
                System.out.println("DEBUG TienDoKhoaHoiService: Phần trăm hoàn thành = " + phanTram + "%");
            }

            // Kiểm tra xem đã hoàn thành toàn bộ khóa học chưa
            if (tienDoKhoaHoc.getSoBoDaHoanThanh() >= tienDoKhoaHoc.getTongSoBo()) {
                tienDoKhoaHoc.setTrangThai("HOAN_THANH");
                tienDoKhoaHoc.setNgayHoanThanh(Instant.now());
            }

            tienDoKhoaHocRepository.save(tienDoKhoaHoc);
            tienDoKhoaHocRepository.flush(); // Đảm bảo commit transaction
            System.out.println("DEBUG TienDoKhoaHoiService: Đã save và flush TienDoKhoaHoc");

            // Tự động unlock bộ câu hỏi tiếp theo (chỉ khi mới đánh dấu HOAN_THANH)
            if (!wasHoanThanh) {
                checkAndUnlockNextBoCauHoi(userId, khoaHocId, boCauHoiId);
            }
        } else if (!wasHoanThanh) {
            // Nếu chưa hoàn thành nhưng đã bắt đầu luyện tập
            System.out.println("DEBUG TienDoKhoaHoiService: Chưa đạt điểm tối thiểu, set DANG_HOC");
            tienDoBoCauHoi.setTrangThai("DANG_HOC");
            tienDoBoCauHoiTrongKhoaRepository.save(tienDoBoCauHoi);
        } else {
            System.out.println("DEBUG TienDoKhoaHoiService: Bộ câu hỏi đã HOAN_THANH nhưng không đạt điểm tối thiểu lần này");
        }
    }

    @Override
    @Transactional
    public TienDoBoCauHoiTrongKhoa getOrCreateBoCauHoiProgress(Long tienDoKhoaHocId, Long boCauHoiId) throws DataNotFoundException {
        TienDoKhoaHoc tienDoKhoaHoc = tienDoKhoaHocRepository.findById(tienDoKhoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Tiến độ khóa học không tồn tại"));

        return tienDoBoCauHoiTrongKhoaRepository
                .findByTienDoKhoaHocIdAndBoCauHoiId(tienDoKhoaHocId, boCauHoiId)
                .orElseGet(() -> {
                    BoCauHoi boCauHoi = null;
                    try {
                        boCauHoi = boCauHoiRepository.findById(boCauHoiId)
                                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));
                    } catch (DataNotFoundException e) {
                        throw new RuntimeException(e);
                    }

                    // Kiểm tra xem user đã unlock chưa
                    boolean daMoKhoa = boCauHoiMoKhoaRepository
                            .existsByNguoiDung_IdAndBoCauHoi_Id(tienDoKhoaHoc.getNguoiDung().getId(), boCauHoiId);

                    TienDoBoCauHoiTrongKhoa tienDoBoCauHoi = TienDoBoCauHoiTrongKhoa.builder()
                            .tienDoKhoaHoc(tienDoKhoaHoc)
                            .boCauHoi(boCauHoi)
                            .trangThai(daMoKhoa ? "DA_MO_KHOA" : "CHUA_MO_KHOA")
                            .diemCaoNhat(0)
                            .soLanLuyenTap(0)
                            .lanCuoiLuyenTap(null)
                            .build();

                    return tienDoBoCauHoiTrongKhoaRepository.save(tienDoBoCauHoi);
                });
    }

    @Override
    @Transactional
    public void checkAndUnlockNextBoCauHoi(Long userId, Long khoaHocId, Long completedBoCauHoiId) throws DataNotFoundException {
        // Tìm bộ câu hỏi vừa hoàn thành trong khóa học
        KhoaHocBoCauHoi completedKhbch = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndBoCauHoiId(khoaHocId, completedBoCauHoiId)
                .orElse(null);

        if (completedKhbch == null) {
            return;
        }

        Integer thuTuHienTai = completedKhbch.getThuTu();

        // Tìm bộ câu hỏi tiếp theo
        KhoaHocBoCauHoi nextKhbch = khoaHocBoCauHoiRepository
                .findByKhoaHocIdAndThuTu(khoaHocId, thuTuHienTai + 1)
                .orElse(null);

        if (nextKhbch == null) {
            // Không còn bộ câu hỏi nào tiếp theo
            return;
        }

        BoCauHoi nextBoCauHoi = nextKhbch.getBoCauHoi();

        // CHỈ tự động unlock nếu user đã unlock khóa học
        boolean daUnlockKhoaHoc = khoaHocMoKhoaRepository
                .existsByNguoiDung_IdAndKhoaHoc_Id(userId, khoaHocId);
        
        if (!daUnlockKhoaHoc) {
            // User chưa unlock khóa học, không tự động unlock bộ câu hỏi tiếp theo
            return;
        }

        // Kiểm tra xem bộ câu hỏi tiếp theo có cần unlock không
        if (Boolean.TRUE.equals(nextBoCauHoi.getCanMoKhoa())
                && nextBoCauHoi.getGiaMoKhoa() != null
                && nextBoCauHoi.getGiaMoKhoa() > 0) {
            // Kiểm tra xem đã unlock chưa
            boolean daMoKhoa = boCauHoiMoKhoaRepository
                    .existsByNguoiDung_IdAndBoCauHoi_Id(userId, nextBoCauHoi.getId());

            if (!daMoKhoa) {
                // Tự động unlock (miễn phí vì đã unlock khóa học và hoàn thành bộ trước)
                BoCauHoiMoKhoa unlock = BoCauHoiMoKhoa.builder()
                        .nguoiDung(nguoiDungRepository.findById(userId)
                                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại")))
                        .boCauHoi(nextBoCauHoi)
                        .moKhoaLuc(Instant.now())
                        .build();
                boCauHoiMoKhoaRepository.save(unlock);
                boCauHoiMoKhoaRepository.flush();

                // Cập nhật trạng thái trong TienDoBoCauHoiTrongKhoa
                TienDoKhoaHoc tienDoKhoaHoc = getOrCreateProgress(userId, khoaHocId);
                TienDoBoCauHoiTrongKhoa tienDoNext = getOrCreateBoCauHoiProgress(
                        tienDoKhoaHoc.getId(), nextBoCauHoi.getId());
                tienDoNext.setTrangThai("DA_MO_KHOA");
                tienDoBoCauHoiTrongKhoaRepository.save(tienDoNext);
            }
        }
    }
}

