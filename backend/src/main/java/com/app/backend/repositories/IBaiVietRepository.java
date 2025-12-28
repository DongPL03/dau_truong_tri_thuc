package com.app.backend.repositories;

import com.app.backend.models.BaiViet;
import com.app.backend.models.constant.TrangThaiBaiViet;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IBaiVietRepository extends JpaRepository<BaiViet, Long> {

    // Lấy bài viết đã duyệt, phân trang
    Page<BaiViet> findByTrangThaiOrderByGhimDescTaoLucDesc(TrangThaiBaiViet trangThai, Pageable pageable);

    // Lấy bài viết của user
    Page<BaiViet> findByNguoiDang_IdAndTrangThaiNotOrderByTaoLucDesc(Long userId, TrangThaiBaiViet trangThai, Pageable pageable);

    // Lấy bài viết theo tag
    @Query("SELECT b FROM BaiViet b JOIN b.tags t WHERE t.id = :tagId AND b.trangThai = :trangThai ORDER BY b.ghim DESC, b.taoLuc DESC")
    Page<BaiViet> findByTagIdAndTrangThai(@Param("tagId") Long tagId, @Param("trangThai") TrangThaiBaiViet trangThai, Pageable pageable);

    // Tìm kiếm bài viết
    @Query("SELECT b FROM BaiViet b WHERE b.trangThai = :trangThai AND (LOWER(b.tieuDe) LIKE LOWER(CONCAT('%', :keyword, '%')) OR LOWER(b.noiDung) LIKE LOWER(CONCAT('%', :keyword, '%'))) ORDER BY b.ghim DESC, b.taoLuc DESC")
    Page<BaiViet> searchByKeyword(@Param("keyword") String keyword, @Param("trangThai") TrangThaiBaiViet trangThai, Pageable pageable);

    // Admin: lấy tất cả bài viết
    Page<BaiViet> findAllByOrderByTaoLucDesc(Pageable pageable);

    // Admin: lấy bài viết theo trạng thái
    Page<BaiViet> findByTrangThaiOrderByTaoLucDesc(TrangThaiBaiViet trangThai, Pageable pageable);

    // Bài viết hot (nhiều like, comment)
    @Query("SELECT b FROM BaiViet b WHERE b.trangThai = 'APPROVED' ORDER BY b.ghim DESC, (b.luotThich + b.luotBinhLuan * 2) DESC, b.taoLuc DESC")
    Page<BaiViet> findHotPosts(Pageable pageable);

    // Đếm bài viết của user
    long countByNguoiDang_IdAndTrangThai(Long userId, TrangThaiBaiViet trangThai);

    // Increment views
    @Modifying
    @Query("UPDATE BaiViet b SET b.luotXem = b.luotXem + 1 WHERE b.id = :id")
    void incrementViews(@Param("id") Long id);

    // Increment likes
    @Modifying
    @Query("UPDATE BaiViet b SET b.luotThich = b.luotThich + 1 WHERE b.id = :id")
    void incrementLikes(@Param("id") Long id);

    // Decrement likes
    @Modifying
    @Query("UPDATE BaiViet b SET b.luotThich = b.luotThich - 1 WHERE b.id = :id AND b.luotThich > 0")
    void decrementLikes(@Param("id") Long id);

    // Increment comments
    @Modifying
    @Query("UPDATE BaiViet b SET b.luotBinhLuan = b.luotBinhLuan + 1 WHERE b.id = :id")
    void incrementComments(@Param("id") Long id);

    // Decrement comments
    @Modifying
    @Query("UPDATE BaiViet b SET b.luotBinhLuan = b.luotBinhLuan - 1 WHERE b.id = :id AND b.luotBinhLuan > 0")
    void decrementComments(@Param("id") Long id);

    // Lấy bài viết với fetch tags
    @Query("SELECT DISTINCT b FROM BaiViet b LEFT JOIN FETCH b.tags WHERE b.id = :id")
    Optional<BaiViet> findByIdWithTags(@Param("id") Long id);
}
