package com.app.backend.controllers;

import com.app.backend.dtos.community.BaoCaoDTO;
import com.app.backend.dtos.community.XuLyBaoCaoDTO;
import com.app.backend.models.NguoiDung;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.community.BaoCaoResponse;
import com.app.backend.services.community.IBaoCaoService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("${api.prefix}/reports")
@RequiredArgsConstructor
public class BaoCaoController {

    private final IBaoCaoService baoCaoService;

    // ============== USER APIs ==============

    /**
     * Báo cáo bài viết
     */
    @PostMapping("/post/{postId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> reportPost(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long postId,
            @Valid @RequestBody BaoCaoDTO dto) {
        try {
            dto.setBaiVietId(postId);
            dto.setBinhLuanId(null);
            BaoCaoResponse response = baoCaoService.createReport(user.getId(), dto);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Báo cáo bài viết thành công")
                            .data(response)
                            .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Báo cáo bình luận
     */
    @PostMapping("/comment/{commentId}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> reportComment(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long commentId,
            @Valid @RequestBody BaoCaoDTO dto) {
        try {
            dto.setBaiVietId(null);
            dto.setBinhLuanId(commentId);
            BaoCaoResponse response = baoCaoService.createReport(user.getId(), dto);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Báo cáo bình luận thành công")
                            .data(response)
                            .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Lấy báo cáo của tôi
     */
    @GetMapping("/my-reports")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getMyReports(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaoCaoResponse> reports = baoCaoService.getMyReports(user.getId(), pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy báo cáo của tôi thành công")
                .data(PageResponse.fromPage(reports))
                .build());
    }

    // ============== ADMIN APIs ==============

    /**
     * Lấy tất cả báo cáo (Admin)
     */
    @GetMapping("/admin/all")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> getAllReports(
            @RequestParam(required = false) String status,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaoCaoResponse> reports = baoCaoService.getAllReports(status, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy danh sách báo cáo thành công")
                .data(PageResponse.fromPage(reports))
                .build());
    }

    /**
     * Lấy báo cáo đang chờ xử lý (Admin)
     */
    @GetMapping("/admin/pending")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> getPendingReports(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaoCaoResponse> reports = baoCaoService.getPendingReports(pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy báo cáo đang chờ xử lý thành công")
                .data(PageResponse.fromPage(reports))
                .build());
    }

    /**
     * Chi tiết báo cáo (Admin)
     */
    @GetMapping("/admin/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> getReportDetail(@PathVariable Long id) {
        try {
            BaoCaoResponse response = baoCaoService.getReportById(id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy chi tiết báo cáo thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.NOT_FOUND)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Xử lý báo cáo (Admin)
     */
    @PostMapping("/admin/{id}/resolve")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> resolveReport(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id,
            @Valid @RequestBody XuLyBaoCaoDTO dto) {
        try {
            BaoCaoResponse response = baoCaoService.resolveReport(admin.getId(), id, dto);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Xử lý báo cáo thành công")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Bỏ qua báo cáo (Admin)
     */
    @PostMapping("/admin/{id}/dismiss")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> dismissReport(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BaoCaoResponse response = baoCaoService.dismissReport(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Đã bỏ qua báo cáo")
                    .data(response)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    /**
     * Thống kê báo cáo (Admin)
     */
    @GetMapping("/admin/stats")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> getReportStats() {
        try {
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy thống kê báo cáo thành công")
                    .data(baoCaoService.getReportStats())
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }
}
