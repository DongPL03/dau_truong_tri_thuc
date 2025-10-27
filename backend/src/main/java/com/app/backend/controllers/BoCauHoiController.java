package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.BoCauHoiDTO;
import com.app.backend.dtos.TuChoiBoCauHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.BoCauHoi;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.bocauhoi.BoCauHoiResponse;
import com.app.backend.services.bocauhoi.IBoCauHoiService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/boCauHoi")
@RequiredArgsConstructor
public class BoCauHoiController {
    private final IBoCauHoiService boCauHoiService;
    private final SecurityUtils securityUtils;

    @GetMapping("")
    public ResponseEntity<ResponseObject> getAll(
            @RequestParam(defaultValue = "") String keyword,
            @RequestParam(defaultValue = "0", name = "chu_de_id") Long chuDeId,
            @RequestParam(defaultValue = "", name = "che_do_hien_thi") String cheDoHienThi,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        PageRequest pageRequest = PageRequest.of(page, limit);
        Page<BoCauHoi> result = boCauHoiService.findAll(
                pageRequest,
                keyword,
                chuDeId,
                cheDoHienThi,
                currentUserId,
                isAdmin
        );
        Page<BoCauHoiResponse> dtoPage = result.map(BoCauHoiResponse::from);

        // Đưa vào wrapper
        PageResponse<BoCauHoiResponse> data = PageResponse.fromPage(dtoPage);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }


    @PostMapping("")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> create(@Valid @RequestBody BoCauHoiDTO boCauHoiDTO, BindingResult result) throws DataNotFoundException {
        if (result.hasErrors()) {
            List<String> errorMessages = result.getFieldErrors()
                    .stream()
                    .map(FieldError::getDefaultMessage)
                    .toList();
            return ResponseEntity.ok().body(ResponseObject.builder()
                    .message(errorMessages.toString())
                    .status(HttpStatus.BAD_REQUEST)
                    .data(null)
                    .build());

        }
        Long userId = securityUtils.getLoggedInUser().getId();
        BoCauHoi boCauHoi = boCauHoiService.create(boCauHoiDTO, userId);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Tao bo cau hoi thanh cong")
                .status(HttpStatus.OK)
                .data(boCauHoi)
                .build());
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> update(@PathVariable Long id, @RequestBody BoCauHoiDTO boCauHoiDTO) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUser().getId();
        boolean isAdmin = securityUtils.isAdmin();
        BoCauHoi boCauHoi = boCauHoiService.update(id, boCauHoiDTO, userId, isAdmin);
        return ResponseEntity.ok().body(ResponseObject.builder()
                .message("Cập nhật bộ câu hỏi thành công")
                .status(HttpStatus.OK)
                .data(boCauHoi)
                .build());
    }

    @PutMapping("/{id}/approve")
    @PreAuthorize("hasRole('ADMIN')")
    public BoCauHoi approve(@PathVariable Long id) {
        // throw nếu không phải admin
        if (securityUtils.requireAdmin()) {
            System.out.println(("Bạn không có quyền ADMIN"));
        }
        Long adminId = securityUtils.getLoggedInUser().getId();
        return boCauHoiService.approve(id, adminId);
    }

    @PutMapping("/{id}/reject")
    @PreAuthorize("hasRole('ADMIN')")
    public BoCauHoi reject(@PathVariable Long id, @RequestBody TuChoiBoCauHoiDTO lyDo) {
        securityUtils.requireAdmin();
        Long adminId = securityUtils.getLoggedInUser().getId();
        return boCauHoiService.reject(id, lyDo, adminId);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER','ADMIN')")
    public ResponseEntity<ResponseObject> delete(@PathVariable Long id) throws DataNotFoundException, PermissionDenyException {
        Long userId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();
        boCauHoiService.softDelete(id, userId, isAdmin);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Xóa bộ câu hỏi thành công")
                        .status(HttpStatus.OK)
                        .data(null)
                        .build()
        );
    }

}
