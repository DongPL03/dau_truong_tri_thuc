package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.*;
import com.app.backend.models.TranDau;
import com.app.backend.responses.ResponseObject;
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
//        return tranDauService.taoPhong(dto, uid);

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
//        return tranDauService.thamGia(dto, uid);
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

    @GetMapping("/{id}")
    public ResponseEntity<ResponseObject> chiTiet(@PathVariable Long id) throws Exception {
        Long uid = securityUtils.getLoggedInUserId();
        TranDau td = tranDauService.chiTietPhong(id, uid);
        TranDauResponse data = TranDauResponse.fromEntity(td);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Lấy thông tin phòng thành công")
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/pending")
    public ResponseEntity<ResponseObject> dsPhongCho(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size) {
        PageRequest pageRequest = PageRequest.of(page, size);
        Page<TranDau> result = tranDauService.danhSachPhongCho(pageRequest);
        Page<TranDauResponse> responseList = result.map(TranDauResponse::fromEntity);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Danh sách phòng đang chờ")
                        .data(responseList)
                        .build()
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
        BattleFinishResponse data = tranDauService.finishBattle(id, uid, false);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .status(HttpStatus.OK)
                        .message("Kết thúc trận đấu thành công")
                        .data(data)
                        .build()
        );
    }
}
