package com.app.backend.services.community;

import com.app.backend.dtos.community.BaiVietDTO;
import com.app.backend.responses.community.BaiVietResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface IBaiVietService {

    // ============== USER APIs ==============

    /**
     * Tạo bài viết mới
     */
    BaiVietResponse createPost(Long userId, BaiVietDTO dto, List<MultipartFile> images) throws Exception;

    /**
     * Cập nhật bài viết
     */
    BaiVietResponse updatePost(Long userId, Long postId, BaiVietDTO dto) throws Exception;

    /**
     * Xóa bài viết (soft delete)
     */
    void deletePost(Long userId, Long postId) throws Exception;

    /**
     * Lấy chi tiết bài viết
     */
    BaiVietResponse getPostById(Long postId, Long currentUserId) throws Exception;

    /**
     * Lấy feed bài viết (đã duyệt)
     */
    Page<BaiVietResponse> getFeed(Long currentUserId, Pageable pageable);

    /**
     * Lấy bài viết hot
     */
    Page<BaiVietResponse> getHotPosts(Long currentUserId, Pageable pageable);

    /**
     * Lấy bài viết theo tag
     */
    Page<BaiVietResponse> getPostsByTag(Long tagId, Long currentUserId, Pageable pageable);

    /**
     * Tìm kiếm bài viết
     */
    Page<BaiVietResponse> searchPosts(String keyword, Long currentUserId, Pageable pageable);

    /**
     * Lấy bài viết của user
     */
    Page<BaiVietResponse> getMyPosts(Long userId, Pageable pageable);

    /**
     * Lấy bài viết đã lưu
     */
    Page<BaiVietResponse> getSavedPosts(Long userId, Pageable pageable);

    /**
     * Like/Unlike bài viết
     */
    boolean toggleLike(Long userId, Long postId) throws Exception;

    /**
     * Lưu/Bỏ lưu bài viết
     */
    boolean toggleSave(Long userId, Long postId) throws Exception;

    /**
     * Tăng lượt xem
     */
    void incrementViews(Long postId);

    // ============== ADMIN APIs ==============

    /**
     * Lấy tất cả bài viết (admin)
     */
    Page<BaiVietResponse> getAllPostsAdmin(String status, Pageable pageable);

    /**
     * Duyệt bài viết
     */
    BaiVietResponse approvePost(Long adminId, Long postId) throws Exception;

    /**
     * Từ chối bài viết
     */
    BaiVietResponse rejectPost(Long adminId, Long postId, String reason) throws Exception;

    /**
     * Ẩn bài viết
     */
    BaiVietResponse hidePost(Long adminId, Long postId) throws Exception;

    /**
     * Ghim/Bỏ ghim bài viết
     */
    BaiVietResponse togglePin(Long adminId, Long postId) throws Exception;
}
