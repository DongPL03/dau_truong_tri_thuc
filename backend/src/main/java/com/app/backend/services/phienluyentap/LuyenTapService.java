package com.app.backend.services.phienluyentap;

import com.app.backend.dtos.BatDauLuyenTapRequestDTO;
import com.app.backend.dtos.TraLoiCauHoiRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.repositories.*;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class LuyenTapService implements ILuyenTapService {
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final ITraLoiLuyenTapRepository traLoiLuyenTapRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ITheGhiNhoRepository theGhiNhoRepository;

    @Override
    @Transactional
    public PhienLuyenTap batDau(BatDauLuyenTapRequestDTO request, Long userId) throws DataNotFoundException {
        BoCauHoi boCauHoi = boCauHoiRepository.findById(request.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // Lấy danh sách câu hỏi của bộ
        List<CauHoi> allQuestions = cauHoiRepository.findByBoCauHoiId(boCauHoi.getId());
        if (allQuestions.isEmpty())
            throw new DataNotFoundException("Bộ câu hỏi chưa có câu hỏi nào");

        // Random n câu
        Collections.shuffle(allQuestions);
        int soLuong = Math.min(request.getSoLuong(), allQuestions.size());
        List<CauHoi> selected = allQuestions.subList(0, soLuong);
        PhienLuyenTap phien = PhienLuyenTap.builder()
                .boCauHoi(boCauHoi)
                .nguoiDung(user)
                .tongCauHoi(selected.size())
                .soCauDung(0)
                .doChinhXac(BigDecimal.ZERO)
                .diemSo(0)
                .thoiGianTbMs(0)
                .build();
        phien.setBoCauHoi(null); // tránh vòng lặp JSON
        return phienLuyenTapRepository.save(phien);
    }

    @Override
    @Transactional
    public PhienLuyenTap guiDapAn(TraLoiCauHoiRequestDTO request, Long userId) throws DataNotFoundException, PermissionDenyException {
        PhienLuyenTap phien = phienLuyenTapRepository.findById(request.getPhienId())
                .orElseThrow(() -> new DataNotFoundException("Phiên luyện tập không tồn tại"));

        if (!phien.getNguoiDung().getId().equals(userId)) {
            throw new PermissionDenyException("Bạn không có quyền nộp bài cho phiên này");
        }

        int correctCount = 0;
        int totalTime = 0;
        int total = request.getCauTraLoiList().size();
        for (TraLoiCauHoiRequestDTO.CauTraLoiRequest ans : request.getCauTraLoiList()) {
            CauHoi cauHoi = cauHoiRepository.findById(ans.getCauHoiId())
                    .orElseThrow(() -> new IllegalArgumentException("Câu hỏi không tồn tại"));

            boolean correct = Character.toUpperCase(cauHoi.getDapAnDung()) ==
                    Character.toUpperCase(ans.getLuaChon());

            if (correct) correctCount++;
            if (ans.getThoiGianMs() != null) totalTime += ans.getThoiGianMs();

            TraLoiLuyenTap traLoi = new TraLoiLuyenTap();
            traLoi.setPhienLuyenTap(phien);
            traLoi.setCauHoi(cauHoi);
            traLoi.setLuaChon(ans.getLuaChon());
            traLoi.setDungHaySai(correct);
            traLoi.setThoiGianMs(ans.getThoiGianMs());
            traLoiLuyenTapRepository.save(traLoi);

//            chiTiet.add(Map.of(
//                    "cauHoiId", cauHoi.getId(),
//                    "dapAnChon", ans.getLuaChon(),
//                    "dungHaySai", correct
//            ));
            if (!correct && !theGhiNhoRepository.existsByPhienIdAndCauHoiId(phien.getId(), cauHoi.getId())) {
                TheGhiNho memo = TheGhiNho.builder()
                        .phien(phien)
                        .cauHoi(cauHoi)
                        .build();
                theGhiNhoRepository.save(memo);
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
        return phienLuyenTapRepository.save(phien);
    }


//    @Override
//    public Map<String, Object> layKetQua(Long phienId, Long userId) throws DataNotFoundException, PermissionDenyException {
//        PhienLuyenTap phien = phienLuyenTapRepository.findById(phienId)
//                .orElseThrow(() -> new DataNotFoundException("Phiên luyện tập không tồn tại"));
//
//        if (!phien.getNguoiDung().getId().equals(userId)) {
//            throw new PermissionDenyException("Bạn không có quyền xem kết quả này");
//        }
//
//        List<TraLoiLuyenTap> traLois = traLoiLuyenTapRepository.findByPhienLuyenTapId(phienId);
//
//        List<Map<String, Object>> chiTiet = traLois.stream()
//                .map(t -> {
//                    Map<String, Object> m = new LinkedHashMap<>();
//                    m.put("cauHoiId", t.getCauHoi().getId());
//                    m.put("noiDung", t.getCauHoi().getNoiDung());
//                    m.put("dapAnDung", t.getCauHoi().getDapAnDung()); // Character
//                    m.put("luaChon", t.getLuaChon());               // Character
//                    m.put("dungHaySai", t.getDungHaySai());            // Boolean
//                    m.put("thoiGianMs", t.getThoiGianMs());            // Integer (có thể null)
//                    return m;
//                })
//                .collect(Collectors.toList());
//
//        Map<String, Object> res = new LinkedHashMap<>();
//        res.put("boCauHoi", phien.getBoCauHoi().getTieuDe());
//        res.put("tongCauHoi", phien.getTongCauHoi());
//        res.put("soCauDung", phien.getSoCauDung());
//        res.put("doChinhXac", phien.getDoChinhXac());
//        res.put("diemSo", phien.getDiemSo());
//        res.put("thoiGianTbMs", phien.getThoiGianTbMs());
//        res.put("chiTiet", chiTiet);
//        res.put("taoLuc", phien.getTaoLuc());
//        return res;
//    }

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

}
