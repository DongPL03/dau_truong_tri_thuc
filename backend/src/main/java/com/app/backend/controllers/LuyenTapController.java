package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BatDauLuyenTapRequestDTO;
import com.app.backend.dtos.TraLoiCauHoiRequestDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.CauHoi;
import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TheGhiNho;
import com.app.backend.models.TraLoiLuyenTap;
import com.app.backend.repositories.ICauHoiRepository;
import com.app.backend.repositories.IPhienLuyenTapRepository;
import com.app.backend.repositories.ITraLoiLuyenTapRepository;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.practice.*;
import com.app.backend.services.phienluyentap.ILuyenTapService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
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
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final SecurityUtils securityUtils;

    @PostMapping("/start")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> batDauLuyenTap(@RequestBody BatDauLuyenTapRequestDTO request) throws DataNotFoundException {
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

    @PostMapping("/submit")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> submitPractice(@RequestBody TraLoiCauHoiRequestDTO request) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        PhienLuyenTap phien = luyenTapService.guiDapAn(request, userId);
        List<TraLoiLuyenTap> traLois = traLoiLuyenTapRepository.findByPhienLuyenTapId(phien.getId());
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Nộp bài luyện tập thành công")
                        .data(SubmitLuyenTapResponse.from(phien, traLois))
                        .build()
        );
//        return ResponseEntity.ok(PracticeSubmitResponse.from(phien, traLois));
    }

    @GetMapping("/{phienId}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> layKetQuaLuyenTap(@PathVariable Long phienId) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        List<TraLoiLuyenTap> traLois = luyenTapService.getTraLoiByPhien(phienId, userId);
        PhienLuyenTap phien = phienLuyenTapRepository.findById(phienId).orElseThrow(() -> new DataNotFoundException("Phiên không tồn tại")); // hoặc load lại từ repo
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
    public ResponseEntity<ResponseObject> getHistory(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size
    ) {
        Long userId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        PageRequest pageRequest = PageRequest.of(page, size);
        Page<PhienLuyenTap> phienPage = luyenTapService.getPracticeHistory(userId, isAdmin, pageRequest);
        Page<LichSuLuyenTapItem> dtoPage = phienPage.map(LichSuLuyenTapItem::from);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy lịch sử luyện tập thành công")
                        .data(dtoPage)
                        .build()
        );
//        return ResponseEntity.ok(phienPage.map(LichSuLuyenTapItem::from));
    }

    @PostMapping("/memo/{phienId}/{cauHoiId}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> saveMemo(@PathVariable Long phienId, @PathVariable Long cauHoiId) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        TheGhiNho memo = luyenTapService.saveTheGhiNho(phienId, cauHoiId, userId);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Đã lưu câu hỏi vào thẻ ghi nhớ")
                        .data(Map.of(
                                "memoId", memo.getId(),
                                "phienId", phienId,
                                "cauHoiId", cauHoiId
                        ))
                        .build()
        );
//        return ResponseEntity.ok(Map.of(
//                "message", "Đã lưu câu hỏi vào thẻ ghi nhớ",
//                "memoId", memo.getId(),
//                "phienId", phienId,
//                "cauHoiId", cauHoiId
//        ));
    }

    @GetMapping("/memo/list")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> getMemoList(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size
    ) {
        Long userId = securityUtils.getLoggedInUserId();
        PageRequest pageable = PageRequest.of(page, size);

        Page<TheGhiNho> memoPage = luyenTapService.getTheGhiNhoList(userId, pageable);
        Page<TheGhiNhoResponse> dtoPage = memoPage.map(TheGhiNhoResponse::from);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách thẻ ghi nhớ thành công")
                        .status(HttpStatus.OK)
                        .data(PageResponse.fromPage(dtoPage))
                        .build()
        );
    }

    // 7️⃣ Xóa thẻ ghi nhớ
    @DeleteMapping("/memo/delete/{memoId}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> deleteMemo(@PathVariable Long memoId) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        luyenTapService.deleteTheGhiNho(memoId, userId);
        return ResponseEntity.ok(ResponseObject.builder()
                .message("Xóa thẻ ghi nhớ thành công")
                .status(HttpStatus.OK)
                .data(null)
                .build());
    }


}
