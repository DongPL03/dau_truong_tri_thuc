package com.app.backend.services.community;

import com.app.backend.dtos.community.BaiVietDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.constant.LoaiBaiViet;
import com.app.backend.models.constant.TrangThaiBaiViet;
import com.app.backend.repositories.*;
import com.app.backend.responses.community.BaiVietResponse;
import com.app.backend.utils.FileUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class BaiVietService implements IBaiVietService {

    private final IBaiVietRepository baiVietRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ITagRepository tagRepository;
    private final ILuotThichRepository luotThichRepository;
    private final IBaiVietLuuRepository baiVietLuuRepository;
    private final IHinhAnhBaiVietRepository hinhAnhBaiVietRepository;
    private final IBangXepHangRepository bangXepHangRepository;

    // Ngưỡng level để đăng bài không cần duyệt (Hybrid moderation)
    private static final int AUTO_APPROVE_LEVEL = 5;

    @Override
    @Transactional
    public BaiVietResponse createPost(Long userId, BaiVietDTO dto, List<MultipartFile> images) throws Exception {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        // Lấy level từ BangXepHang
        int userLevel = bangXepHangRepository.findByNguoiDung_Id(userId)
                .map(BangXepHang::getLevel)
                .orElse(1);

        // Xác định loại bài
        LoaiBaiViet loaiBai = LoaiBaiViet.THAO_LUAN;
        if (dto.loai() != null) {
            try {
                loaiBai = LoaiBaiViet.valueOf(dto.loai().toUpperCase());
            } catch (IllegalArgumentException ignored) {}
        }

        // Hybrid moderation: user level cao đăng thẳng, level thấp cần duyệt
        TrangThaiBaiViet trangThai = userLevel >= AUTO_APPROVE_LEVEL 
                ? TrangThaiBaiViet.APPROVED 
                : TrangThaiBaiViet.PENDING;

        BaiViet baiViet = BaiViet.builder()
                .nguoiDang(user)
                .tieuDe(dto.tieuDe())
                .noiDung(dto.noiDung())
                .loaiBai(loaiBai)
                .trangThai(trangThai)
                .build();

        // Thêm tags
        if (dto.tagIds() != null && !dto.tagIds().isEmpty()) {
            List<Tag> tags = tagRepository.findAllByIdIn(dto.tagIds());
            baiViet.setTags(tags);
            // Increment post count cho mỗi tag
            tags.forEach(tag -> tagRepository.incrementPostCount(tag.getId()));
        }

        BaiViet saved = baiVietRepository.save(baiViet);

        // Upload images from MultipartFile
        int order = 0;
        if (images != null && !images.isEmpty()) {
            for (MultipartFile image : images) {
                if (!image.isEmpty()) {
                    String fileName = FileUtils.storeFile(image, "HINH_ANH");
                    HinhAnhBaiViet hinhAnh = HinhAnhBaiViet.builder()
                            .baiViet(saved)
                            .duongDan(fileName)
                            .thuTu(order++)
                            .build();
                    hinhAnhBaiVietRepository.save(hinhAnh);
                }
            }
        }

        // Add images from URLs (already uploaded)
        if (dto.hinhAnhUrls() != null && !dto.hinhAnhUrls().isEmpty()) {
            for (String url : dto.hinhAnhUrls()) {
                if (url != null && !url.isBlank()) {
                    HinhAnhBaiViet hinhAnh = HinhAnhBaiViet.builder()
                            .baiViet(saved)
                            .duongDan(url)
                            .thuTu(order++)
                            .build();
                    hinhAnhBaiVietRepository.save(hinhAnh);
                }
            }
        }

        return BaiVietResponse.fromEntity(saved, false, false, userLevel, userId);
    }

    @Override
    @Transactional
    public BaiVietResponse updatePost(Long userId, Long postId, BaiVietDTO dto) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        if (!baiViet.getNguoiDang().getId().equals(userId)) {
            throw new IllegalStateException("Bạn không có quyền sửa bài viết này");
        }

        baiViet.setTieuDe(dto.tieuDe());
        baiViet.setNoiDung(dto.noiDung());

        if (dto.loai() != null) {
            try {
                baiViet.setLoaiBai(LoaiBaiViet.valueOf(dto.loai().toUpperCase()));
            } catch (IllegalArgumentException ignored) {}
        }

        // Update tags
        if (dto.tagIds() != null) {
            // Decrement old tags
            baiViet.getTags().forEach(tag -> tagRepository.decrementPostCount(tag.getId()));
            // Set new tags
            List<Tag> newTags = tagRepository.findAllByIdIn(dto.tagIds());
            baiViet.setTags(newTags);
            // Increment new tags
            newTags.forEach(tag -> tagRepository.incrementPostCount(tag.getId()));
        }

        BaiViet saved = baiVietRepository.save(baiViet);

        // Update images from URLs (already uploaded)
        if (dto.hinhAnhUrls() != null) {
            // Remove old images
            hinhAnhBaiVietRepository.deleteAllByBaiViet_Id(postId);

            // Add new images
            int order = 0;
            for (String url : dto.hinhAnhUrls()) {
                if (url != null && !url.isBlank()) {
                    HinhAnhBaiViet hinhAnh = HinhAnhBaiViet.builder()
                            .baiViet(saved)
                            .duongDan(url)
                            .thuTu(order++)
                            .build();
                    hinhAnhBaiVietRepository.save(hinhAnh);
                }
            }
        }

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(userId)
                .map(BangXepHang::getLevel)
                .orElse(1);

        boolean daThich = luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(postId, userId);
        boolean daLuu = baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(postId, userId);

        return BaiVietResponse.fromEntity(saved, daThich, daLuu, userLevel, userId);
    }

    @Override
    @Transactional
    public void deletePost(Long userId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        if (!baiViet.getNguoiDang().getId().equals(userId)) {
            throw new IllegalStateException("Bạn không có quyền xóa bài viết này");
        }

        // Decrement tag counts
        baiViet.getTags().forEach(tag -> tagRepository.decrementPostCount(tag.getId()));

        // Soft delete
        baiViet.setTrangThai(TrangThaiBaiViet.DELETED);
        baiVietRepository.save(baiViet);
    }

    @Override
    @Transactional(readOnly = true)
    public BaiVietResponse getPostById(Long postId, Long currentUserId) throws Exception {
        BaiViet baiViet = baiVietRepository.findByIdWithTags(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        if (baiViet.getTrangThai() == TrangThaiBaiViet.DELETED) {
            throw new DataNotFoundException("Bài viết đã bị xóa");
        }

        boolean daThich = currentUserId != null && 
                luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(postId, currentUserId);
        boolean daLuu = currentUserId != null && 
                baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(postId, currentUserId);

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                .map(BangXepHang::getLevel)
                .orElse(1);

        return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, currentUserId);
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getFeed(Long currentUserId, Pageable pageable) {
        Page<BaiViet> posts = baiVietRepository.findByTrangThaiOrderByGhimDescTaoLucDesc(
                TrangThaiBaiViet.APPROVED, pageable);

        return posts.map(baiViet -> {
            boolean daThich = currentUserId != null && 
                    luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            boolean daLuu = currentUserId != null && 
                    baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, currentUserId);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getHotPosts(Long currentUserId, Pageable pageable) {
        Page<BaiViet> posts = baiVietRepository.findHotPosts(pageable);

        return posts.map(baiViet -> {
            boolean daThich = currentUserId != null && 
                    luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            boolean daLuu = currentUserId != null && 
                    baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, currentUserId);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getPostsByTag(Long tagId, Long currentUserId, Pageable pageable) {
        Page<BaiViet> posts = baiVietRepository.findByTagIdAndTrangThai(
                tagId, TrangThaiBaiViet.APPROVED, pageable);

        return posts.map(baiViet -> {
            boolean daThich = currentUserId != null && 
                    luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            boolean daLuu = currentUserId != null && 
                    baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, currentUserId);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> searchPosts(String keyword, Long currentUserId, Pageable pageable) {
        Page<BaiViet> posts = baiVietRepository.searchByKeyword(
                keyword, TrangThaiBaiViet.APPROVED, pageable);

        return posts.map(baiViet -> {
            boolean daThich = currentUserId != null && 
                    luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            boolean daLuu = currentUserId != null && 
                    baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), currentUserId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, currentUserId);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getMyPosts(Long userId, Pageable pageable) {
        Page<BaiViet> posts = baiVietRepository.findByNguoiDang_IdAndTrangThaiNotOrderByTaoLucDesc(
                userId, TrangThaiBaiViet.DELETED, pageable);

        return posts.map(baiViet -> {
            boolean daThich = luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), userId);
            boolean daLuu = baiVietLuuRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), userId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, daLuu, userLevel, userId);
        });
    }

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getSavedPosts(Long userId, Pageable pageable) {
        Page<BaiVietLuu> savedPosts = baiVietLuuRepository.findByNguoiDung_IdOrderByTaoLucDesc(userId, pageable);

        return savedPosts.map(saved -> {
            BaiViet baiViet = saved.getBaiViet();
            boolean daThich = luotThichRepository.existsByBaiViet_IdAndNguoiDung_Id(baiViet.getId(), userId);
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, daThich, true, userLevel, userId);
        });
    }

    @Override
    @Transactional
    public boolean toggleLike(Long userId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        var existingLike = luotThichRepository.findByBaiViet_IdAndNguoiDung_Id(postId, userId);

        if (existingLike.isPresent()) {
            // Unlike
            luotThichRepository.delete(existingLike.get());
            baiVietRepository.decrementLikes(postId);
            return false;
        } else {
            // Like
            LuotThich like = LuotThich.builder()
                    .baiViet(baiViet)
                    .nguoiDung(user)
                    .build();
            luotThichRepository.save(like);
            baiVietRepository.incrementLikes(postId);
            return true;
        }
    }

    @Override
    @Transactional
    public boolean toggleSave(Long userId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        var existingSave = baiVietLuuRepository.findByBaiViet_IdAndNguoiDung_Id(postId, userId);

        if (existingSave.isPresent()) {
            // Unsave
            baiVietLuuRepository.delete(existingSave.get());
            return false;
        } else {
            // Save
            BaiVietLuu save = BaiVietLuu.builder()
                    .baiViet(baiViet)
                    .nguoiDung(user)
                    .build();
            baiVietLuuRepository.save(save);
            return true;
        }
    }

    @Override
    @Transactional
    public void incrementViews(Long postId) {
        baiVietRepository.incrementViews(postId);
    }

    // ============== ADMIN ==============

    @Override
    @Transactional(readOnly = true)
    public Page<BaiVietResponse> getAllPostsAdmin(String status, Pageable pageable) {
        Page<BaiViet> posts;

        if (status != null && !status.isEmpty()) {
            try {
                TrangThaiBaiViet trangThai = TrangThaiBaiViet.valueOf(status.toUpperCase());
                posts = baiVietRepository.findByTrangThaiOrderByTaoLucDesc(trangThai, pageable);
            } catch (IllegalArgumentException e) {
                posts = baiVietRepository.findAllByOrderByTaoLucDesc(pageable);
            }
        } else {
            posts = baiVietRepository.findAllByOrderByTaoLucDesc(pageable);
        }

        return posts.map(baiViet -> {
            int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                    .map(BangXepHang::getLevel)
                    .orElse(1);
            return BaiVietResponse.fromEntity(baiViet, false, false, userLevel);
        });
    }

    @Override
    @Transactional
    public BaiVietResponse approvePost(Long adminId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        baiViet.setTrangThai(TrangThaiBaiViet.APPROVED);
        baiViet.setDuyetBoi(admin);
        baiViet.setDuyetLuc(Instant.now());

        BaiViet saved = baiVietRepository.save(baiViet);

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                .map(BangXepHang::getLevel)
                .orElse(1);

        return BaiVietResponse.fromEntity(saved, false, false, userLevel);
    }

    @Override
    @Transactional
    public BaiVietResponse rejectPost(Long adminId, Long postId, String reason) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        NguoiDung admin = nguoiDungRepository.findById(adminId)
                .orElseThrow(() -> new DataNotFoundException("Admin không tồn tại"));

        baiViet.setTrangThai(TrangThaiBaiViet.REJECTED);
        baiViet.setDuyetBoi(admin);
        baiViet.setDuyetLuc(Instant.now());

        BaiViet saved = baiVietRepository.save(baiViet);

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                .map(BangXepHang::getLevel)
                .orElse(1);

        return BaiVietResponse.fromEntity(saved, false, false, userLevel);
    }

    @Override
    @Transactional
    public BaiVietResponse hidePost(Long adminId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        baiViet.setTrangThai(TrangThaiBaiViet.HIDDEN);

        BaiViet saved = baiVietRepository.save(baiViet);

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                .map(BangXepHang::getLevel)
                .orElse(1);

        return BaiVietResponse.fromEntity(saved, false, false, userLevel);
    }

    @Override
    @Transactional
    public BaiVietResponse togglePin(Long adminId, Long postId) throws Exception {
        BaiViet baiViet = baiVietRepository.findById(postId)
                .orElseThrow(() -> new DataNotFoundException("Bài viết không tồn tại"));

        baiViet.setGhim(!baiViet.getGhim());

        BaiViet saved = baiVietRepository.save(baiViet);

        int userLevel = bangXepHangRepository.findByNguoiDung_Id(baiViet.getNguoiDang().getId())
                .map(BangXepHang::getLevel)
                .orElse(1);

        return BaiVietResponse.fromEntity(saved, false, false, userLevel);
    }
}
