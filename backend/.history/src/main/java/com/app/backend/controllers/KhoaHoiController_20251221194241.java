package com.app.backend.controllers;

import com.app.backend.components.SecurityUtils;
import com.app.backend.dtos.KhoaHoiDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.KhoaHoc;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.khoahoc.KhoaHoiDetailResponse;
import com.app.backend.responses.khoahoc.KhoaHoiResponse;
import com.app.backend.responses.khoahoc.UnlockKhoaHocResponse;
import com.app.backend.services.khoahoc.IKhoaHoiService;
import com.app.backend.services.khoahoc.KhoaHoiService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("${api.prefix}/khoa-hoc")
@RequiredArgsConstructor
public class KhoaHoiController {

    private final IKhoaHoiService khoaHoiService;
    private final KhoaHoiService khoaHoiServiceImpl; // Để gọi method unlockKhoaHoc
    private final SecurityUtils securityUtils;

    @GetMapping("")
    public ResponseEntity<ResponseObject> getAll(
            @RequestParam(defaultValue = "") String keyword,
            @RequestParam(defaultValue = "0", name = "chu_de_id") Long chuDeId,
            @RequestParam(defaultValue = "", name = "trang_thai") String trangThai,
            @RequestParam(defaultValue = "NEWEST") String sort_order,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit
    ) {
        Long currentUserId = securityUtils.getLoggedInUserId();
        boolean isAdmin = securityUtils.isAdmin();

        Sort sort = sort_order.equalsIgnoreCase("OLDEST")
                ? Sort.by(Sort.Direction.ASC, "taoLuc")
                : Sort.by(Sort.Direction.DESC, "taoLuc");
        PageRequest pageRequest = PageRequest.of(page, limit, sort);

        Page<KhoaHoc> result = khoaHoiService.findAll(
                pageRequest,
                keyword,
                chuDeId,
                trangThai,
                currentUserId,
                isAdmin
        );

        Page<KhoaHoiResponse> dtoPage = result.map(KhoaHoiResponse::from);

        PageResponse<KhoaHoiResponse> data = PageResponse.fromPage(dtoPage);
        return ResponseEntity.ok(
                ResponseObject.builder()
                        .message("Lấy danh sách khóa học thành công")
                        .status(HttpStatus.OK)
                        .data(data)
                        .build()
        );
    }

    @GetMapping("/{id}")
    public ResponseEntity<ResponseObject> getById(@PathVariable Long id) {
        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            boolean isAdmin = securityUtils.isAdmin();

            KhoaHoiDetailResponse detail = khoaHoiService.getDetailById(id, currentUserId, isAdmin);

            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .message("Lấy chi tiết khóa học thành công")
                            .status(HttpStatus.OK)
                            .data(detail)
                            .build()
            );
        } catch (DataNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.NOT_FOUND)
                            .data(null)
                            .build());
        } catch (PermissionDenyException e) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.FORBIDDEN)
                            .data(null)
                            .build());
        }
    }

    @PostMapping("")
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> create(
            @Valid @RequestBody KhoaHoiDTO khoaHoiDTO,
            BindingResult result
    ) {
        if (result.hasErrors()) {
            Map<String, String> errors = new HashMap<>();
            for (FieldError error : result.getFieldErrors()) {
                errors.put(error.getField(), error.getDefaultMessage());
            }
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .message("Dữ liệu không hợp lệ")
                            .status(HttpStatus.BAD_REQUEST)
                            .data(errors)
                            .build());
        }

        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            KhoaHoc khoaHoc = khoaHoiService.create(khoaHoiDTO, currentUserId);

            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .message("Tạo khóa học thành công")
                            .status(HttpStatus.CREATED)
                            .data(KhoaHoiResponse.from(khoaHoc))
                            .build());
        } catch (DataNotFoundException | PermissionDenyException e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.BAD_REQUEST)
                            .data(null)
                            .build());
        }
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> update(
            @PathVariable Long id,
            @Valid @RequestBody KhoaHoiDTO khoaHoiDTO,
            BindingResult result
    ) {
        if (result.hasErrors()) {
            Map<String, String> errors = new HashMap<>();
            for (FieldError error : result.getFieldErrors()) {
                errors.put(error.getField(), error.getDefaultMessage());
            }
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .message("Dữ liệu không hợp lệ")
                            .status(HttpStatus.BAD_REQUEST)
                            .data(errors)
                            .build());
        }

        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            boolean isAdmin = securityUtils.isAdmin();
            KhoaHoc khoaHoc = khoaHoiService.update(id, khoaHoiDTO, currentUserId, isAdmin);

            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .message("Cập nhật khóa học thành công")
                            .status(HttpStatus.OK)
                            .data(KhoaHoiResponse.from(khoaHoc))
                            .build());
        } catch (DataNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.NOT_FOUND)
                            .data(null)
                            .build());
        } catch (PermissionDenyException e) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.FORBIDDEN)
                            .data(null)
                            .build());
        }
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> delete(@PathVariable Long id) {
        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            boolean isAdmin = securityUtils.isAdmin();
            khoaHoiService.delete(id, currentUserId, isAdmin);

            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .message("Xóa khóa học thành công")
                            .status(HttpStatus.OK)
                            .data(null)
                            .build());
        } catch (DataNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.NOT_FOUND)
                            .data(null)
                            .build());
        } catch (PermissionDenyException e) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.FORBIDDEN)
                            .data(null)
                            .build());
        }
    }

    @DeleteMapping("/{id}/soft")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public ResponseEntity<ResponseObject> softDelete(@PathVariable Long id) {
        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            boolean isAdmin = securityUtils.isAdmin();
            khoaHoiService.softDelete(id, currentUserId, isAdmin);

            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .message("Xóa mềm khóa học thành công")
                            .status(HttpStatus.OK)
                            .data(null)
                            .build());
        } catch (DataNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.NOT_FOUND)
                            .data(null)
                            .build());
        } catch (PermissionDenyException e) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.FORBIDDEN)
                            .data(null)
                            .build());
        }
    }

    @PutMapping("/{id}/unlock")
    public ResponseEntity<ResponseObject> unlockKhoaHoc(@PathVariable Long id) {
        try {
            Long currentUserId = securityUtils.getLoggedInUserId();
            UnlockKhoaHocResponse response = khoaHoiServiceImpl.unlockKhoaHoc(id, currentUserId);

            return ResponseEntity.ok(
                    ResponseObject.builder()
                            .message("Mở khóa khóa học thành công")
                            .status(HttpStatus.OK)
                            .data(response)
                            .build());
        } catch (DataNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.NOT_FOUND)
                            .data(null)
                            .build());
        } catch (IllegalStateException e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .message(e.getMessage())
                            .status(HttpStatus.BAD_REQUEST)
                            .data(null)
                            .build());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ResponseObject.builder()
                            .message("Lỗi khi mở khóa khóa học: " + e.getMessage())
                            .status(HttpStatus.INTERNAL_SERVER_ERROR)
                            .data(null)
                            .build());
        }
    }
}

