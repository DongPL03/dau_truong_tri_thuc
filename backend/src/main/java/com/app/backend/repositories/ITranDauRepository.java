package com.app.backend.repositories;

import com.app.backend.models.TranDau;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ITranDauRepository extends JpaRepository<TranDau, Long> {
    Page<TranDau> findByTrangThai(String trangThai, Pageable pageable);
}
