package com.app.backend.services.community;

import com.app.backend.dtos.community.TagDTO;
import com.app.backend.responses.community.TagResponse;

import java.util.List;

public interface ITagService {

    /**
     * Lấy tất cả tags đang hiển thị
     */
    List<TagResponse> getAllTags();

    /**
     * Lấy tất cả tags (admin)
     */
    List<TagResponse> getAllTagsAdmin();

    /**
     * Lấy tag theo ID
     */
    TagResponse getTagById(Long id) throws Exception;

    /**
     * Lấy tag theo slug
     */
    TagResponse getTagBySlug(String slug) throws Exception;

    /**
     * Tạo tag mới (Admin)
     */
    TagResponse createTag(TagDTO dto) throws Exception;

    /**
     * Cập nhật tag (Admin)
     */
    TagResponse updateTag(Long id, TagDTO dto) throws Exception;

    /**
     * Xóa tag (Admin)
     */
    void deleteTag(Long id) throws Exception;

    /**
     * Lấy tags phổ biến (nhiều bài viết nhất)
     */
    List<TagResponse> getPopularTags(int limit);

    /**
     * Tìm kiếm tag theo tên
     */
    List<TagResponse> searchTags(String keyword);

    /**
     * Ẩn/Hiện tag (Admin)
     */
    TagResponse toggleVisibility(Long id) throws Exception;
}
