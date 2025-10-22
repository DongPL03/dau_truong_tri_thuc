package com.app.backend.repositories;

import com.app.backend.models.BangXepHang;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IBangXepHangRepository extends JpaRepository<BangXepHang, Long> {
}
