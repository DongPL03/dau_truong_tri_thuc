package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BatDauLuyenTapRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.CauHoi;
import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TheGhiNho;
import com.app.backend.models.TraLoiLuyenTap;
import com.app.backend.repositories.ICauHoiRepository;
import com.app.backend.repositories.ITraLoiLuyenTapRepository;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.practice.LichSuLuyenTapItem;
import com.app.backend.responses.practice.KetQuaLuyenTapResponse;
import com.app.backend.responses.practice.BatDauLuyenTapResponse;
import com.app.backend.services.phienluyentap.ILuyenTapService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("${api.prefix}/luyenTap")
@RequiredArgsConstructor
public class LuyenTapController {
    private final ILuyenTapService luyenTapService;
    private final ICauHoiRepository cauHoiRepository;
    private final ITraLoiLuyenTapRepository traLoiLuyenTapRepository;
    private final SecurityUtils securityUtils;

    @PostMapping("/start")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> startPractice(@RequestBody BatDauLuyenTapRequestDTO request) throws DataNotFoundException {
        Long userId = securityUtils.getLoggedInUserId();
        PhienLuyenTap phien = luyenTapService.batDau(request, userId);
        List<CauHoi> selected = cauHoiRepository.findByBoCauHoiId(phien.getBoCauHoi().getId());
//        return ResponseEntity.ok(PracticeStartResponse.from(phien, selected));
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Bắt đầu phiên luyện tập thành công")
                        .data(BatDauLuyenTapResponse.from(phien, selected))
                        .build()
        );
    }

    @GetMapping("/{phienId}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> getPracticeResult(@PathVariable Long phienId) {
        Long userId = securityUtils.getLoggedInUserId();
        List<TraLoiLuyenTap> traLois = luyenTapService.getTraLoiByPhien(phienId, userId);
        PhienLuyenTap phien = traLois.get(0).getPhienLuyenTap(); // hoặc load lại từ repo
//        return ResponseEntity.ok(PracticeResultResponse.from(phien, traLois));
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy kết quả luyện tập thành công")
                        .data(KetQuaLuyenTapResponse.from(phien, traLois))
                        .build()
        );
    }

    @GetMapping("/history")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<Page<LichSuLuyenTapItem>> getHistory(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size
    ) {
        Long userId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        Page<PhienLuyenTap> phienPage = luyenTapService.getPracticeHistory(userId, isAdmin, page, size);
        return ResponseEntity.ok(phienPage.map(LichSuLuyenTapItem::from));
    }

    @PostMapping("/memo/{phienId}/{cauHoiId}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<Map<String, Object>> saveMemo(@PathVariable Long phienId, @PathVariable Long cauHoiId) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        TheGhiNho memo = luyenTapService.saveTheGhiNho(phienId, cauHoiId, userId);
        return ResponseEntity.ok(Map.of(
                "message", "Đã lưu câu hỏi vào thẻ ghi nhớ",
                "memoId", memo.getId(),
                "phienId", phienId,
                "cauHoiId", cauHoiId
        ));
    }
}
