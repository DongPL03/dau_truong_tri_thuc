package com.app.backend.repositories;

import com.app.backend.models.BinhLuan;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IBinhLuanRepository extends JpaRepository<BinhLuan, Long> {

    // Lấy comments gốc (không phải reply) của bài viết
    @Query("SELECT b FROM BinhLuan b WHERE b.baiViet.id = :baiVietId AND b.binhLuanCha IS NULL AND b.biAn = false ORDER BY b.taoLuc DESC")
    Page<BinhLuan> findRootCommentsByBaiVietId(@Param("baiVietId") Long baiVietId, Pageable pageable);

    // Lấy replies của một comment
    @Query("SELECT b FROM BinhLuan b WHERE b.binhLuanCha.id = :parentId AND b.biAn = false ORDER BY b.taoLuc ASC")
    List<BinhLuan> findRepliesByParentId(@Param("parentId") Long parentId);

    // Đếm số comment của bài viết
    long countByBaiViet_IdAndBiAnFalse(Long baiVietId);

    // Đếm số replies của comment
    long countByBinhLuanCha_IdAndBiAnFalse(Long parentId);

    // Lấy tất cả comments của user
    Page<BinhLuan> findByNguoiBinhLuan_IdAndBiAnFalseOrderByTaoLucDesc(Long userId, Pageable pageable);

    // Admin: lấy tất cả comments của bài viết
    @Query("SELECT b FROM BinhLuan b WHERE b.baiViet.id = :baiVietId ORDER BY b.taoLuc DESC")
    Page<BinhLuan> findAllByBaiVietId(@Param("baiVietId") Long baiVietId, Pageable pageable);

    // Increment likes
    @Modifying
    @Query("UPDATE BinhLuan b SET b.luotThich = b.luotThich + 1 WHERE b.id = :id")
    void incrementLikes(@Param("id") Long id);

    // Decrement likes
    @Modifying
    @Query("UPDATE BinhLuan b SET b.luotThich = b.luotThich - 1 WHERE b.id = :id AND b.luotThich > 0")
    void decrementLikes(@Param("id") Long id);

    // Xóa tất cả comments của bài viết
    void deleteAllByBaiViet_Id(Long baiVietId);
}
