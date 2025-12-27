package com.app.backend.repositories;

import com.app.backend.models.KhoaHocMoKhoa;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface IKhoaHocMoKhoaRepository extends JpaRepository<KhoaHocMoKhoa, Long> {

    boolean existsByNguoiDung_IdAndKhoaHoc_Id(Long nguoiDungId, Long khoaHocId);

    Optional<KhoaHocMoKhoa> findByNguoiDung_IdAndKhoaHoc_Id(Long userId, Long khoaHocId);

    List<KhoaHocMoKhoa> findAllByNguoiDung_Id(Long userId);
}

