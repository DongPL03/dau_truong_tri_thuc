package com.app.backend.services.community;

import com.app.backend.dtos.community.BinhLuanDTO;
import com.app.backend.responses.community.BinhLuanResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface IBinhLuanService {

    /**
     * Tạo bình luận mới (comment hoặc reply)
     */
    BinhLuanResponse createComment(Long userId, BinhLuanDTO dto) throws Exception;

    /**
     * Cập nhật bình luận
     */
    BinhLuanResponse updateComment(Long userId, Long commentId, BinhLuanDTO dto) throws Exception;

    /**
     * Xóa bình luận
     */
    void deleteComment(Long userId, Long commentId) throws Exception;

    /**
     * Lấy danh sách comments gốc của bài viết (không bao gồm replies)
     */
    Page<BinhLuanResponse> getCommentsByPost(Long postId, Long currentUserId, Pageable pageable);

    /**
     * Lấy replies của một comment
     */
    Page<BinhLuanResponse> getReplies(Long commentId, Long currentUserId, Pageable pageable);

    /**
     * Like/Unlike bình luận
     */
    boolean toggleLike(Long userId, Long commentId) throws Exception;

    /**
     * Ẩn bình luận (Admin)
     */
    BinhLuanResponse hideComment(Long adminId, Long commentId) throws Exception;

    /**
     * Hiện lại bình luận đã ẩn (Admin)
     */
    BinhLuanResponse unhideComment(Long adminId, Long commentId) throws Exception;
}
