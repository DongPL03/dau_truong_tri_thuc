package com.app.backend.repositories;

import com.app.backend.models.KhoaHoc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface IKhoaHocRepository extends JpaRepository<KhoaHoc, Long> {

    @Query("""
            SELECT k FROM KhoaHoc k
            WHERE k.isXoa = false
            AND (:keyword IS NULL OR :keyword = '' OR LOWER(k.tieuDe) LIKE %:keyword%)
            AND (:chuDeId IS NULL OR :chuDeId = 0 OR k.chuDe.id = :chuDeId)
            AND (:trangThai IS NULL OR :trangThai = '' OR k.trangThai = :trangThai)
            AND (
                 :isAdmin = true
                 OR k.taoBoi.id = :creatorId
                 OR k.trangThai = 'PUBLISHED'
            )
            ORDER BY k.thuTu ASC, k.taoLuc DESC
            """)
    Page<KhoaHoc> searchKhoaHoc(Pageable pageable,
                                @Param("keyword") String keyword,
                                @Param("chuDeId") Long chuDeId,
                                @Param("trangThai") String trangThai,
                                @Param("creatorId") Long creatorId,
                                @Param("isAdmin") boolean isAdmin);

    @Query("""
            SELECT k FROM KhoaHoc k
            WHERE k.isXoa = false
            AND k.trangThai = 'PUBLISHED'
            AND (:chuDeId IS NULL OR :chuDeId = 0 OR k.chuDe.id = :chuDeId)
            ORDER BY k.thuTu ASC, k.taoLuc DESC
            """)
    Page<KhoaHoc> findPublishedKhoaHoc(Pageable pageable,
                                       @Param("chuDeId") Long chuDeId);

    List<KhoaHoc> findByChuDeIdAndIsXoaFalseOrderByThuTuAsc(Long chuDeId);

    Optional<KhoaHoc> findByIdAndIsXoaFalse(Long id);

    long countByTrangThai(String trangThai);
}

