package com.app.backend.repositories;

import com.app.backend.models.PhienLuyenTap;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IPhienLuyenTapRepository extends JpaRepository<PhienLuyenTap, Long> {
    Page<PhienLuyenTap> findByNguoiDungId(Long userId, Pageable pageable);
}
