package com.app.backend.services.community;

import com.app.backend.dtos.community.TagDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.Tag;
import com.app.backend.repositories.ITagRepository;
import com.app.backend.responses.community.TagResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.Normalizer;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
public class TagService implements ITagService {

    private final ITagRepository tagRepository;

    private static final Pattern NONLATIN = Pattern.compile("[^\\w-]");
    private static final Pattern WHITESPACE = Pattern.compile("[\\s]");

    @Override
    @Transactional(readOnly = true)
    public List<TagResponse> getAllTags() {
        return tagRepository.findByHienThiTrueOrderByThuTuAscTenAsc()
                .stream()
                .map(TagResponse::fromEntity)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public List<TagResponse> getAllTagsAdmin() {
        return tagRepository.findAll()
                .stream()
                .map(TagResponse::fromEntity)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public TagResponse getTagById(Long id) throws Exception {
        Tag tag = tagRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Tag không tồn tại với id: " + id));
        return TagResponse.fromEntity(tag);
    }

    @Override
    @Transactional(readOnly = true)
    public TagResponse getTagBySlug(String slug) throws Exception {
        Tag tag = tagRepository.findBySlug(slug)
                .orElseThrow(() -> new DataNotFoundException("Tag không tồn tại với slug: " + slug));
        return TagResponse.fromEntity(tag);
    }

    @Override
    @Transactional
    public TagResponse createTag(TagDTO dto) throws Exception {
        // Validate tên không trùng
        if (tagRepository.existsByTen(dto.ten())) {
            throw new IllegalArgumentException("Tên tag đã tồn tại");
        }

        // Tạo slug từ tên nếu không có
        String slug = dto.slug();
        if (slug == null || slug.isBlank()) {
            slug = toSlug(dto.ten());
        }

        // Validate slug không trùng
        if (tagRepository.existsBySlug(slug)) {
            throw new IllegalArgumentException("Slug tag đã tồn tại");
        }

        Tag tag = Tag.builder()
                .ten(dto.ten())
                .slug(slug)
                .moTa(dto.moTa())
                .mauSac(dto.mauSac() != null ? dto.mauSac() : "#6366F1")
                .icon(dto.icon())
                .thuTu(dto.thuTu() != null ? dto.thuTu() : 0)
                .hienThi(dto.hienThi() != null ? dto.hienThi() : true)
                .soBaiViet(0)
                .build();

        Tag saved = tagRepository.save(tag);
        return TagResponse.fromEntity(saved);
    }

    @Override
    @Transactional
    public TagResponse updateTag(Long id, TagDTO dto) throws Exception {
        Tag tag = tagRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Tag không tồn tại với id: " + id));

        // Check tên không trùng với tag khác
        if (!tag.getTen().equals(dto.ten()) && tagRepository.existsByTen(dto.ten())) {
            throw new IllegalArgumentException("Tên tag đã tồn tại");
        }

        // Xử lý slug
        String newSlug = dto.slug();
        if (newSlug == null || newSlug.isBlank()) {
            newSlug = toSlug(dto.ten());
        }

        // Check slug không trùng với tag khác
        if (!tag.getSlug().equals(newSlug) && tagRepository.existsBySlug(newSlug)) {
            throw new IllegalArgumentException("Slug tag đã tồn tại");
        }

        tag.setTen(dto.ten());
        tag.setSlug(newSlug);
        tag.setMoTa(dto.moTa());
        if (dto.mauSac() != null) {
            tag.setMauSac(dto.mauSac());
        }
        if (dto.icon() != null) {
            tag.setIcon(dto.icon());
        }
        if (dto.thuTu() != null) {
            tag.setThuTu(dto.thuTu());
        }
        if (dto.hienThi() != null) {
            tag.setHienThi(dto.hienThi());
        }

        Tag saved = tagRepository.save(tag);
        return TagResponse.fromEntity(saved);
    }

    @Override
    @Transactional
    public void deleteTag(Long id) throws Exception {
        Tag tag = tagRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Tag không tồn tại với id: " + id));

        // Soft delete: chỉ ẩn đi, không xóa hẳn
        tag.setHienThi(false);
        tagRepository.save(tag);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TagResponse> getPopularTags(int limit) {
        return tagRepository.findTopPopularTags(limit)
                .stream()
                .map(TagResponse::fromEntity)
                .toList();
    }

    @Override
    @Transactional(readOnly = true)
    public List<TagResponse> searchTags(String keyword) {
        if (keyword == null || keyword.isBlank()) {
            return List.of();
        }
        return tagRepository.searchByName(keyword.trim())
                .stream()
                .map(TagResponse::fromEntity)
                .toList();
    }

    @Override
    @Transactional
    public TagResponse toggleVisibility(Long id) throws Exception {
        Tag tag = tagRepository.findById(id)
                .orElseThrow(() -> new DataNotFoundException("Tag không tồn tại với id: " + id));

        tag.setHienThi(!tag.getHienThi());
        Tag saved = tagRepository.save(tag);
        return TagResponse.fromEntity(saved);
    }

    /**
     * Chuyển text thành slug
     * "Toán Học" -> "toan-hoc"
     */
    private String toSlug(String input) {
        String nowhitespace = WHITESPACE.matcher(input).replaceAll("-");
        String normalized = Normalizer.normalize(nowhitespace, Normalizer.Form.NFD);
        String slug = NONLATIN.matcher(normalized).replaceAll("");
        return slug.toLowerCase(Locale.ENGLISH).replaceAll("-+", "-").replaceAll("^-|-$", "");
    }
}
