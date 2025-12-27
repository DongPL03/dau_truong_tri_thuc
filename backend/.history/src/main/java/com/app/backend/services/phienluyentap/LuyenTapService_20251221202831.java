package com.app.backend.services.phienluyentap;

import com.app.backend.dtos.BatDauLuyenTapRequestDTO;
import com.app.backend.dtos.TraLoiCauHoiRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.models.constant.CheDoHienThi;
import com.app.backend.models.constant.TrangThaiBoCauHoi;
import com.app.backend.repositories.*;
import com.app.backend.responses.luyentap.BatDauLuyenTapResponse;
import com.app.backend.responses.luyentap.SubmitLuyenTapResponse;
import com.app.backend.services.khoahoc.ITienDoKhoaHoiService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
public class LuyenTapService implements ILuyenTapService {
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final ITraLoiLuyenTapRepository traLoiLuyenTapRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ITheGhiNhoRepository theGhiNhoRepository;
    private final IBoCauHoiMoKhoaRepository boCauHoiMoKhoaRepository;
    private final ITienDoKhoaHoiService tienDoKhoaHoiService;

    @Override
    @Transactional
    public BatDauLuyenTapResponse batDau(BatDauLuyenTapRequestDTO request, Long userId)
            throws DataNotFoundException, PermissionDenyException {

        BoCauHoi boCauHoi = boCauHoiRepository.findById(request.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // Không cho luyện tập trên bộ official (dùng cho thi đấu)
        if (Boolean.TRUE.equals(boCauHoi.getIsOfficial())) {
            throw new PermissionDenyException("Không thể luyện tập với bộ câu hỏi Official dùng cho thi đấu");
        }

        // Bắt buộc bộ đã được duyệt
        if (!TrangThaiBoCauHoi.DA_DUYET.equals(boCauHoi.getTrangThai())) {
            throw new PermissionDenyException("Bộ câu hỏi chưa được duyệt");
        }

        // Nếu bộ PRIVATE thì chỉ cho chủ bộ luyện tập
        if (CheDoHienThi.PRIVATE.equals(boCauHoi.getCheDoHienThi())
                && !boCauHoi.getTaoBoi().getId().equals(user.getId())) {
            throw new PermissionDenyException("Bạn không có quyền luyện tập với bộ câu hỏi riêng tư này");
        }

        // Nếu bộ yêu cầu mở khoá bằng vàng thì bắt buộc user đã mở khoá (server-side guard)
        // TRỪ KHI: user là admin hoặc là chủ bộ
        boolean isOwner = boCauHoi.getTaoBoi() != null && boCauHoi.getTaoBoi().getId().equals(userId);
        boolean isAdmin = user.getVaiTro() != null && "admin".equalsIgnoreCase(user.getVaiTro().getTenVaiTro());
        
        if (Boolean.TRUE.equals(boCauHoi.getCanMoKhoa())
                && boCauHoi.getGiaMoKhoa() != null
                && boCauHoi.getGiaMoKhoa() > 0
                && !isOwner
                && !isAdmin) {
            boolean daMoKhoa = boCauHoiMoKhoaRepository.existsByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoi.getId());
            if (!daMoKhoa) {
                throw new PermissionDenyException("Bộ câu hỏi này cần được mở khóa bằng vàng trước khi luyện tập");
            }
        }


        List<CauHoi> allQuestions = cauHoiRepository.findByBoCauHoiId(boCauHoi.getId());
        if (allQuestions.isEmpty())
            throw new DataNotFoundException("Bộ câu hỏi chưa có câu hỏi nào");

        Collections.shuffle(allQuestions);
        int soLuong = allQuestions.size();
        PhienLuyenTap phien = PhienLuyenTap.builder()
                .boCauHoi(boCauHoi)
                .nguoiDung(user)
                .tongCauHoi(soLuong)
                .soCauDung(0)
                .doChinhXac(BigDecimal.ZERO)
                .diemSo(0)
                .thoiGianTbMs(0)
                .build();

        phien = phienLuyenTapRepository.save(phien);
        return BatDauLuyenTapResponse.from(phien, allQuestions);
    }


    @Override
    @Transactional
    public SubmitLuyenTapResponse guiDapAn(TraLoiCauHoiRequestDTO request, Long userId)
            throws DataNotFoundException, PermissionDenyException {

        PhienLuyenTap phien = phienLuyenTapRepository.findById(request.getPhienId())
                .orElseThrow(() -> new DataNotFoundException("Phiên luyện tập không tồn tại"));

        if (!phien.getNguoiDung().getId().equals(userId)) {
            throw new PermissionDenyException("Bạn không có quyền nộp bài cho phiên này");
        }

        int correctCount = 0;
        int totalTime = 0;
        int total = request.getCauTraLoiList().size();
        
        // Kiểm tra xem có phải practice từ memo không (tongCauHoi < tổng số câu hỏi trong bộ)
        BoCauHoi boCauHoi = phien.getBoCauHoi();
        boolean isPracticeFromMemo = false;
        if (boCauHoi != null && phien.getTongCauHoi() != null 
                && boCauHoi.getSoCauHoi() != null 
                && phien.getTongCauHoi() < boCauHoi.getSoCauHoi()) {
            isPracticeFromMemo = true;
            System.out.println("DEBUG guiDapAn: Phát hiện practice từ memo - tongCauHoi=" 
                + phien.getTongCauHoi() + ", soCauHoi=" + boCauHoi.getSoCauHoi());
        }

        for (TraLoiCauHoiRequestDTO.CauTraLoiRequest ans : request.getCauTraLoiList()) {
            CauHoi cauHoi = cauHoiRepository.findById(ans.getCauHoiId())
                    .orElseThrow(() -> new IllegalArgumentException("Câu hỏi không tồn tại"));

            boolean correct = ans.getLuaChon() != null &&
                    Character.toUpperCase(cauHoi.getDapAnDung()) ==
                            Character.toUpperCase(ans.getLuaChon());

            if (correct) correctCount++;
            if (ans.getThoiGianMs() != null) {
                totalTime += ans.getThoiGianMs();
            }

            TraLoiLuyenTap traLoi = TraLoiLuyenTap.builder()
                    .phienLuyenTap(phien)
                    .cauHoi(cauHoi)
                    .luaChon(ans.getLuaChon())
                    .dungHaySai(correct)
                    .thoiGianMs(ans.getThoiGianMs())
                    .build();
            traLoiLuyenTapRepository.save(traLoi);

            if (!correct && !theGhiNhoRepository.existsByPhienIdAndCauHoiId(phien.getId(), cauHoi.getId())) {
                // Lưu vào the_ghi_nho nếu trả lời sai
                TheGhiNho memo = TheGhiNho.builder()
                        .phien(phien)
                        .cauHoi(cauHoi)
                        .build();
                theGhiNhoRepository.save(memo);
            } else if (correct && isPracticeFromMemo) {
                // Nếu là practice từ memo và trả lời đúng, xóa the_ghi_nho của câu hỏi này
                List<TheGhiNho> memosToDelete = theGhiNhoRepository.findAllByUserIdAndBoCauHoiId(
                        userId, boCauHoi.getId())
                        .stream()
                        .filter(m -> m.getCauHoi().getId().equals(cauHoi.getId()))
                        .collect(java.util.stream.Collectors.toList());
                
                if (!memosToDelete.isEmpty()) {
                    System.out.println("DEBUG guiDapAn: Xóa " + memosToDelete.size() 
                        + " the_ghi_nho của câu hỏi ID=" + cauHoi.getId() 
                        + " vì đã trả lời đúng trong practice từ memo");
                    theGhiNhoRepository.deleteAll(memosToDelete);
                    theGhiNhoRepository.flush(); // Đảm bảo xóa ngay
                }
            }
        }

        BigDecimal doChinhXac = (total == 0)
                ? BigDecimal.ZERO
                : BigDecimal.valueOf(correctCount * 100.0 / total)
                .setScale(2, RoundingMode.HALF_UP);

        phien.setSoCauDung(correctCount);
        phien.setDiemSo(correctCount);
        phien.setDoChinhXac(doChinhXac);
        phien.setThoiGianTbMs(total == 0 ? 0 : totalTime / total);
        phienLuyenTapRepository.save(phien);

        // Cập nhật progress trong khóa học (nếu bộ câu hỏi thuộc một khóa học)
        try {
            System.out.println("DEBUG LuyenTapService.guiDapAn: Bắt đầu cập nhật progress - userId=" + userId 
                + ", boCauHoiId=" + phien.getBoCauHoi().getId() 
                + ", correctCount=" + correctCount);
            tienDoKhoaHoiService.updateProgressAfterPractice(
                    userId,
                    phien.getBoCauHoi().getId(),
                    correctCount,
                    doChinhXac
            );
            System.out.println("DEBUG LuyenTapService.guiDapAn: Đã cập nhật progress thành công");
        } catch (Exception e) {
            // Log lỗi nhưng không throw để không ảnh hưởng đến kết quả practice
            System.err.println("❌ Lỗi khi cập nhật progress khóa học: " + e.getMessage());
            e.printStackTrace(); // In full stack trace để debug
        }

        List<TraLoiLuyenTap> traLois = traLoiLuyenTapRepository.findByPhienLuyenTapId(phien.getId());
        return SubmitLuyenTapResponse.from(phien, traLois);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TraLoiLuyenTap> getTraLoiByPhien(Long phienId, Long userId) throws DataNotFoundException, PermissionDenyException {
        PhienLuyenTap phien = phienLuyenTapRepository.findById(phienId)
                .orElseThrow(() -> new DataNotFoundException("Phiên luyện tập không tồn tại"));
        if (!phien.getNguoiDung().getId().equals(userId))
            throw new PermissionDenyException("Bạn không có quyền xem kết quả này");
        return traLoiLuyenTapRepository.findByPhienLuyenTapId(phienId);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<PhienLuyenTap> getPracticeHistory(Long userId, boolean isAdmin, PageRequest pageRequest) {
        return isAdmin ? phienLuyenTapRepository.findAll(pageRequest) : phienLuyenTapRepository.findByNguoiDungId(userId, pageRequest);
    }

    @Override
    public TheGhiNho saveTheGhiNho(Long phienId, Long cauHoiId, Long userId) throws DataNotFoundException, PermissionDenyException {
        PhienLuyenTap phien = phienLuyenTapRepository.findById(phienId)
                .orElseThrow(() -> new DataNotFoundException("Phiên luyện tập không tồn tại"));
        if (!phien.getNguoiDung().getId().equals(userId))
            throw new PermissionDenyException("Bạn không thể thêm ghi nhớ cho người khác");

        CauHoi cauHoi = cauHoiRepository.findById(cauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Câu hỏi không tồn tại"));

        if (theGhiNhoRepository.existsByPhienIdAndCauHoiId(phienId, cauHoiId))
            throw new DataNotFoundException("Câu hỏi này đã được ghi nhớ trước đó");

        TheGhiNho memo = TheGhiNho.builder()
                .phien(phien)
                .cauHoi(cauHoi)
                .build();
        return theGhiNhoRepository.save(memo);
    }

    @Override
    public Page<TheGhiNho> getTheGhiNhoList(Long userId, Pageable pageable) {
        return theGhiNhoRepository.findByPhien_NguoiDung_Id(userId, pageable);
    }

    @Override
    @Transactional
    public void deleteTheGhiNho(Long memoId, Long userId) throws DataNotFoundException, PermissionDenyException {
        TheGhiNho memo = theGhiNhoRepository.findById(memoId)
                .orElseThrow(() -> new DataNotFoundException("Thẻ ghi nhớ không tồn tại"));

        Long ownerId = memo.getPhien().getNguoiDung().getId();
        if (!ownerId.equals(userId)) {
            throw new PermissionDenyException("Bạn không có quyền xóa thẻ ghi nhớ này");
        }

        theGhiNhoRepository.delete(memo);
    }

    @Override
    @Transactional
    public BatDauLuyenTapResponse batDauTuTheGhiNho(Long boCauHoiId, Long userId) throws DataNotFoundException {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        BoCauHoi boCauHoi = boCauHoiRepository.findById(boCauHoiId)
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        // Nếu bộ yêu cầu mở khoá bằng vàng thì bắt buộc user đã mở khoá (server-side guard)
        // TRỪ KHI: user là admin hoặc là chủ bộ
        boolean isOwner = boCauHoi.getTaoBoi() != null && boCauHoi.getTaoBoi().getId().equals(userId);
        boolean isAdmin = user.getVaiTro() != null && "admin".equalsIgnoreCase(user.getVaiTro().getTenVaiTro());
        
        if (Boolean.TRUE.equals(boCauHoi.getCanMoKhoa())
                && boCauHoi.getGiaMoKhoa() != null
                && boCauHoi.getGiaMoKhoa() > 0
                && !isOwner
                && !isAdmin) {
            boolean daMoKhoa = boCauHoiMoKhoaRepository.existsByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoi.getId());
            if (!daMoKhoa) {
                throw new DataNotFoundException("Bộ câu hỏi này cần được mở khóa bằng vàng trước khi luyện tập từ thẻ ghi nhớ");
            }
        }

        // Lấy tất cả thẻ ghi nhớ của user cho bộ này
        List<TheGhiNho> memos = theGhiNhoRepository.findAllByUserIdAndBoCauHoiId(userId, boCauHoiId);
        if (memos.isEmpty()) {
            throw new DataNotFoundException("Bạn chưa có thẻ ghi nhớ nào cho bộ câu hỏi này");
        }

        // Lấy danh sách câu hỏi distinct từ memos (chỉ lấy các câu hỏi đã trả lời sai)
        List<CauHoi> selected = memos.stream()
                .map(TheGhiNho::getCauHoi)
                .distinct()
                .collect(java.util.stream.Collectors.toList());

        System.out.println("DEBUG batDauTuTheGhiNho: Tổng số the_ghi_nho = " + memos.size() 
            + ", Số câu hỏi distinct = " + selected.size() 
            + ", boCauHoiId = " + boCauHoiId 
            + ", userId = " + userId);

        // Xáo trộn cho đỡ nhàm chán
        java.util.Collections.shuffle(selected);

        // Tạo phiên luyện tập mới
        PhienLuyenTap phien = PhienLuyenTap.builder()
                .boCauHoi(boCauHoi)
                .nguoiDung(user)
                .tongCauHoi(selected.size())
                .soCauDung(0)
                .doChinhXac(java.math.BigDecimal.ZERO)
                .diemSo(0)
                .thoiGianTbMs(0)
                .build();

        phien = phienLuyenTapRepository.save(phien);

        // Trả về response giống /start bình thường
        return BatDauLuyenTapResponse.from(phien, selected);
    }

}
