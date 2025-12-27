package com.app.backend.repositories;

import com.app.backend.models.TienDoKhoaHoc;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface ITienDoKhoaHocRepository extends JpaRepository<TienDoKhoaHoc, Long> {

    Optional<TienDoKhoaHoc> findByNguoiDungIdAndKhoaHocId(Long nguoiDungId, Long khoaHocId);

    List<TienDoKhoaHoc> findByNguoiDungIdOrderByCapNhatLucDesc(Long nguoiDungId);

    @Query("""
            SELECT tdk FROM TienDoKhoaHoc tdk
            WHERE tdk.nguoiDung.id = :nguoiDungId
            AND tdk.trangThai IN ('DANG_HOC', 'HOAN_THANH')
            ORDER BY tdk.capNhatLuc DESC
            """)
    List<TienDoKhoaHoc> findActiveKhoaHocByUserId(@Param("nguoiDungId") Long nguoiDungId);

    boolean existsByNguoiDungIdAndKhoaHocId(Long nguoiDungId, Long khoaHocId);
}

