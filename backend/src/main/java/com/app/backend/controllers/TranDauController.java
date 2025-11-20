package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.*;
import com.app.backend.models.TranDau;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.trandau.LichSuTranDauDetailResponse;
import com.app.backend.responses.trandau.BattleFinishResponse;
import com.app.backend.responses.trandau.BattleStartResponse;
import com.app.backend.responses.trandau.SubmitAnswerResponse;
import com.app.backend.responses.trandau.TranDauResponse;
import com.app.backend.services.trandau.ITranDauService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("${api.prefix}/tranDau")
@RequiredArgsConstructor
public class TranDauController {

    private final ITranDauService tranDauService;
    private final SecurityUtils securityUtils;

    @PostMapping("/create")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> taoPhong(@Valid @RequestBody TaoTranDauDTO dto) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        TranDau tranDau = tranDauService.taoPhong(dto, uid);
        TranDauResponse data = TranDauResponse.fromEntity(tranDau);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Tạo phòng thành công")
                        .data(data)
                        .build()
        );

    }


    @PostMapping("/join")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> thamGia(@Valid @RequestBody ThamGiaTranDauDTO dto) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        TranDau tranDau = tranDauService.thamGia(dto, uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Tham gia phòng thành công")
                        .data(tranDau)
                        .build()
        );
    }

    @PostMapping("/leave")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> roiPhong(@Valid @RequestBody RoiTranDauDTO dto) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        tranDauService.roiPhong(dto, uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .data(null)
                        .message("Rời phòng thành công")
                        .build()
        );
    }

    //    @GetMapping("/{id}")
//    public ResponseEntity<ResponseObject> chiTiet(@PathVariable Long id) throws Exception {
//        TranDau td = tranDauService.chiTietPhong(id);
//        TranDauResponse data = TranDauResponse.fromEntity(td);
//        return ResponseEntity.ok(
//                ResponseObject.builder()
//                        .status(HttpStatus.OK)
//                        .message("Lấy thông tin phòng thành công")
//                        .data(data)
//                        .build()
//        );
//    }
    @GetMapping("/{id}")
    public ResponseEntity<ResponseObject> getBattleDetail(@PathVariable Long id) throws Exception {
        // Gọi hàm service mới trả về DTO
        TranDauResponse response = tranDauService.getBattleDetailResponse(id);

        return ResponseEntity.ok(ResponseObject.<TranDauResponse>builder()
                .message("Lấy thông tin phòng thành công")
                .status(HttpStatus.OK)
                .data(response)
                .build());
    }

    @GetMapping("/pending")
    public ResponseEntity<ResponseObject> dsPhongCho(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size) {
        PageRequest pageRequest = PageRequest.of(page, size);
        Page<TranDau> result = tranDauService.danhSachPhongCho(pageRequest);
        Page<TranDauResponse> responseList = result.map(TranDauResponse::fromEntity);
        PageResponse<TranDauResponse> data = PageResponse.fromPage(responseList);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Danh sách phòng đang chờ")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/sync/{id}")
//    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> sync(@PathVariable Long id) throws Exception {
        Long uid = securityUtils.getLoggedInUserIdSafe();
        var data = tranDauService.syncState(id, uid);
        return ResponseEntity.ok(
                ResponseObject.builder().status(HttpStatus.OK)
                        .message("Đồng bộ trạng thái thành công").data(data).build()
        );
    }

    @PutMapping("/start/{id}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> startBattle(@PathVariable("id") Long tranDauId) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        BattleStartResponse data = tranDauService.startBattle(tranDauId, uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Trận đấu đã được bắt đầu thành công")
                        .data(data)
                        .build()
        );
    }

    @PostMapping("/submit-answer")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> submitAnswer(@Valid @RequestBody SubmitAnswerDTO dto) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        SubmitAnswerResponse data = tranDauService.submitAnswer(dto, uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Gửi đáp án thành công")
                        .data(data)
                        .build()
        );
    }

    @PutMapping("/finish/{id}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> finishBattle(@PathVariable Long id) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        System.out.println(">>> [CTRL] finishBattle called, tranDauId=" + id + ", currentUserId=" + uid);
        BattleFinishResponse data = tranDauService.finishBattle(id, uid, false);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Kết thúc trận đấu thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/history/my")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyHistory(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long uid = securityUtils.getLoggedInUserId();
        Page<LichSuTranDauResponse> result = tranDauService.getMyHistory(uid, page, limit);

        PageResponse<LichSuTranDauResponse> pageRes = PageResponse.fromPage(result);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy lịch sử trận đấu thành công")
                        .data(pageRes)
                        .build()
        );
    }

    @GetMapping("/history/my/{tranDauId}")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> getMyHistoryDetail(
            @PathVariable("tranDauId") Long tranDauId
    ) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        LichSuTranDauDetailResponse data = tranDauService.getMyHistoryDetail(tranDauId, uid);

        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy chi tiết lịch sử trận đấu thành công")
                        .data(data)
                        .build()
        );
    }

    @PostMapping("/chat")
    @PreAuthorize("hasRole('ROLE_USER')")
    public ResponseEntity<ResponseObject> guiChat(@Valid @RequestBody GuiChatDTO dto) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        tranDauService.guiChatTrongTran(dto, uid);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Đã gửi tin nhắn")
                        .build()
        );
    }

}
