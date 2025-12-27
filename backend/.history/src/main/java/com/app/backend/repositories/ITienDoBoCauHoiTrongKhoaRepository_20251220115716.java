package com.app.backend.repositories;

import com.app.backend.models.TienDoBoCauHoiTrongKhoa;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface ITienDoBoCauHoiTrongKhoaRepository extends JpaRepository<TienDoBoCauHoiTrongKhoa, Long> {

    Optional<TienDoBoCauHoiTrongKhoa> findByTienDoKhoaHocIdAndBoCauHoiId(Long tienDoKhoaHocId, Long boCauHoiId);

    List<TienDoBoCauHoiTrongKhoa> findByTienDoKhoaHocIdOrderByIdAsc(Long tienDoKhoaHocId);

    @Query("""
            SELECT tdbch FROM TienDoBoCauHoiTrongKhoa tdbch
            WHERE tdbch.tienDoKhoaHoc.id = :tienDoKhoaHocId
            AND tdbch.trangThai = 'HOAN_THANH'
            """)
    List<TienDoBoCauHoiTrongKhoa> findCompletedBoCauHoi(@Param("tienDoKhoaHocId") Long tienDoKhoaHocId);
}

