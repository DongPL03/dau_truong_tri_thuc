package com.app.backend.controllers;

import com.app.backend.dtos.community.BaiVietDTO;
import com.app.backend.models.NguoiDung;
import com.app.backend.responses.PageResponse;
import com.app.backend.responses.ResponseObject;
import com.app.backend.responses.community.BaiVietResponse;
import com.app.backend.services.community.IBaiVietService;
import com.app.backend.utils.FileUtils;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@RestController
@RequestMapping("${api.prefix}/posts")
@RequiredArgsConstructor
public class BaiVietController {

    private final IBaiVietService baiVietService;

    // ============== USER APIs ==============

    /**
     * Tạo bài viết mới
     */
    @PostMapping(consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> createPost(
            @AuthenticationPrincipal NguoiDung user,
            @RequestPart("data") @Valid BaiVietDTO dto,
            @RequestPart(value = "images", required = false) List<MultipartFile> images) {
        try {
            BaiVietResponse response = baiVietService.createPost(user.getId(), dto, images);
            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ResponseObject.builder()
                            .status(HttpStatus.CREATED)
                            .message("Tạo bài viết thành công")
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
     * Upload ảnh cho bài viết (dùng trong Quill editor)
     */
    @PostMapping(value = "/upload-image", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> uploadImage(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam("file") MultipartFile file) {
        try {
            if (file == null || file.isEmpty()) {
                throw new Exception("File không được để trống");
            }

            if (!FileUtils.isImageFile(file)) {
                throw new Exception("Chỉ chấp nhận file ảnh (jpg, png, gif, webp)");
            }

            // Lưu file và trả về tên file
            String filename = FileUtils.storeFile(file, "HINH_ANH");

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Upload ảnh thành công")
                    .data(filename)
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
     * Cập nhật bài viết
     */
    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> updatePost(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id,
            @Valid @RequestBody BaiVietDTO dto) {
        try {
            BaiVietResponse response = baiVietService.updatePost(user.getId(), id, dto);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Cập nhật bài viết thành công")
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
     * Xóa bài viết
     */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> deletePost(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            baiVietService.deletePost(user.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Xóa bài viết thành công")
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
     * Lấy chi tiết bài viết
     */
    @GetMapping("/{id}")
    public ResponseEntity<ResponseObject> getPostById(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            Long userId = user != null ? user.getId() : null;
            BaiVietResponse response = baiVietService.getPostById(id, userId);

            // Increment views
            baiVietService.incrementViews(id);

            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Lấy bài viết thành công")
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
     * Lấy feed bài viết
     */
    @GetMapping("/feed")
    public ResponseEntity<ResponseObject> getFeed(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getFeed(userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy feed thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Lấy bài viết hot
     */
    @GetMapping("/hot")
    public ResponseEntity<ResponseObject> getHotPosts(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getHotPosts(userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy bài viết hot thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Lấy bài viết theo tag
     */
    @GetMapping("/tag/{tagId}")
    public ResponseEntity<ResponseObject> getPostsByTag(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long tagId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getPostsByTag(tagId, userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy bài viết theo tag thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Tìm kiếm bài viết
     */
    @GetMapping("/search")
    public ResponseEntity<ResponseObject> searchPosts(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Long userId = user != null ? user.getId() : null;
        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.searchPosts(keyword, userId, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Tìm kiếm thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Lấy bài viết của tôi
     */
    @GetMapping("/my-posts")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getMyPosts(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getMyPosts(user.getId(), pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy bài viết của tôi thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Lấy bài viết đã lưu
     */
    @GetMapping("/saved")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> getSavedPosts(
            @AuthenticationPrincipal NguoiDung user,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getSavedPosts(user.getId(), pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy bài viết đã lưu thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Like/Unlike bài viết
     */
    @PostMapping("/{id}/like")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> toggleLike(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            boolean liked = baiVietService.toggleLike(user.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(liked ? "Đã thích bài viết" : "Đã bỏ thích bài viết")
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

    /**
     * Lưu/Bỏ lưu bài viết
     */
    @PostMapping("/{id}/save")
    @PreAuthorize("hasAnyRole('USER', 'ADMIN')")
    public ResponseEntity<ResponseObject> toggleSave(
            @AuthenticationPrincipal NguoiDung user,
            @PathVariable Long id) {
        try {
            boolean saved = baiVietService.toggleSave(user.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(saved ? "Đã lưu bài viết" : "Đã bỏ lưu bài viết")
                    .data(saved)
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
     * Lấy tất cả bài viết (Admin)
     */
    @GetMapping("/admin/all")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> getAllPostsAdmin(
            @RequestParam(required = false) String status,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int limit) {

        Pageable pageable = PageRequest.of(page, limit);
        Page<BaiVietResponse> posts = baiVietService.getAllPostsAdmin(status, pageable);

        return ResponseEntity.ok(ResponseObject.builder()
                .status(HttpStatus.OK)
                .message("Lấy danh sách bài viết thành công")
                .data(PageResponse.fromPage(posts))
                .build());
    }

    /**
     * Duyệt bài viết (Admin)
     */
    @PostMapping("/admin/{id}/approve")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> approvePost(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BaiVietResponse response = baiVietService.approvePost(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Duyệt bài viết thành công")
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
     * Từ chối bài viết (Admin)
     */
    @PostMapping("/admin/{id}/reject")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> rejectPost(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id,
            @RequestParam(required = false) String reason) {
        try {
            BaiVietResponse response = baiVietService.rejectPost(admin.getId(), id, reason);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Từ chối bài viết thành công")
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
     * Ẩn bài viết (Admin)
     */
    @PostMapping("/admin/{id}/hide")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> hidePost(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BaiVietResponse response = baiVietService.hidePost(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message("Ẩn bài viết thành công")
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
     * Ghim/Bỏ ghim bài viết (Admin)
     */
    @PostMapping("/admin/{id}/pin")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ResponseObject> togglePin(
            @AuthenticationPrincipal NguoiDung admin,
            @PathVariable Long id) {
        try {
            BaiVietResponse response = baiVietService.togglePin(admin.getId(), id);
            return ResponseEntity.ok(ResponseObject.builder()
                    .status(HttpStatus.OK)
                    .message(response.getGhim() ? "Ghim bài viết thành công" : "Bỏ ghim bài viết thành công")
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
     * Serve ảnh bài viết
     */
    @GetMapping("/images/{imageName}")
    public ResponseEntity<?> getImage(@PathVariable String imageName) {
        try {
            java.nio.file.Path imagePath = java.nio.file.Paths.get("uploads/images/" + imageName);

            if (!java.nio.file.Files.exists(imagePath)) {
                return ResponseEntity.notFound().build();
            }

            org.springframework.core.io.UrlResource resource = new org.springframework.core.io.UrlResource(imagePath.toUri());
            if (resource.exists()) {
                String contentType = java.nio.file.Files.probeContentType(imagePath);
                if (contentType == null) {
                    contentType = "application/octet-stream";
                }
                return ResponseEntity.ok()
                        .contentType(MediaType.parseMediaType(contentType))
                        .body(resource);
            }
            return ResponseEntity.notFound().build();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
}
