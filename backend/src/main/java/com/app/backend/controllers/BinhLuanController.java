package com.app.backend.controllers;

import com.app.backend.dtos.community.BinhLuanDTO;
import com.app.backend.models.NguoiDung;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.community.BinhLuanResponse;
import com.app.backend.services.community.IBinhLuanService;
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

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/comments")
@RequiredArgsConstructor
public class BinhLuanController {

    private final IBinhLuanService binhLuanService;

    /**
     * Tạo bình luận mới
     */
    @PostMapping
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> createComment(
            @AuthenticationPrincipal NguoiDung user,
            @Valid @RequestBody BinhLuanDTO dto) {
        try {
            BinhLuanResponse response = binhLuanService.createComment(user.getId(), dto);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Tạo bình luận thành công")
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
     * Cập nhật bình luận
     */
    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> updateComment(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id,
            @Valid @RequestBody BinhLuanDTO dto) {
        try {
            BinhLuanResponse response = binhLuanService.updateComment(user.getId(), id, dto);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Cập nhật bình luận thành công")
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
     * Xóa bình luận
     */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> deleteComment(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            binhLuanService.deleteComment(user.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Xóa bình luận thành công")
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
     * Lấy bình luận của bài viết (root comments với replies)
     */
    @GetMapping("/post/{postId}")
    public ResponseEntity<ResponseObject> getCommentsByPost(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long postId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BinhLuanResponse> comments = binhLuanService.getCommentsByPost(postId, userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy bình luận thành công")
                .data(PageResponse.fromPage(comments))
                .build());
    }

    /**
     * Lấy replies của một bình luận
     */
    @GetMapping("/{commentId}/replies")
    public ResponseEntity<ResponseObject> getReplies(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long commentId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BinhLuanResponse> replies = binhLuanService.getReplies(commentId, userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy replies thành công")
                .data(PageResponse.fromPage(replies))
                .build());
    }

    /**
     * Like/Unlike bình luận
     */
    @PostMapping("/{id}/like")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> toggleLike(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            boolean liked = binhLuanService.toggleLike(user.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(liked ? "Đã thích bình luận" : "Đã bỏ thích bình luận")
                    .data(liked)
                    .build());
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ResponseObject.builder()
                            .status(HttpStatus.BAD_REQUEST)
                            .message(e.getMessage())
                            .build());
        }
    }

    // ============== ADMIN APIs ==============

    /**
     * Ẩn bình luận (Admin)
     */
    @PostMapping("/admin/{id}/hide")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> hideComment(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BinhLuanResponse response = binhLuanService.hideComment(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Ẩn bình luận thành công")
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
     * Hiện lại bình luận đã ẩn (Admin)
     */
    @PostMapping("/admin/{id}/unhide")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> unhideComment(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BinhLuanResponse response = binhLuanService.unhideComment(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Hiện lại bình luận thành công")
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
}
