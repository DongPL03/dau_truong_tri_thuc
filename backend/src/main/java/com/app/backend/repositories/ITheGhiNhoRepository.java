package com.app.backend.repositories;

import com.app.backend.models.TheGhiNho;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ITheGhiNhoRepository extends JpaRepository<TheGhiNho, Long> {
    List<TheGhiNho> findByPhienId(Long phienId);

    boolean existsByPhienIdAndCauHoiId(Long phienId, Long cauHoiId);

    Page<TheGhiNho> findByPhien_NguoiDung_Id(Long nguoiDungId, Pageable pageable);

    @Query("""
            SELECT m FROM TheGhiNho m
            WHERE m.phien.nguoiDung.id = :userId
            ORDER BY m.taoLuc DESC
            """)
    List<TheGhiNho> findAllByUserId(@Param("userId") Long userId);

    @Query("""
            SELECT m FROM TheGhiNho m
            WHERE m.phien.nguoiDung.id = :userId
            AND m.phien.boCauHoi.id = :boCauHoiId
            ORDER BY m.taoLuc DESC
            """)
    List<TheGhiNho> findAllByUserIdAndBoCauHoiId(@Param("userId") Long userId,
                                                 @Param("boCauHoiId") Long boCauHoiId);
}
