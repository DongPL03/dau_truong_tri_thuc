package com.app.backend.repositories;

import com.app.backend.models.PhanTichHocTap;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface IPhanTichHocTapRepository extends JpaRepository<PhanTichHocTap, Long> {

    Optional<PhanTichHocTap> findByNguoiDungIdAndKhoaHocId(Long nguoiDungId, Long khoaHocId);

    boolean existsByNguoiDungIdAndKhoaHocId(Long nguoiDungId, Long khoaHocId);
}

