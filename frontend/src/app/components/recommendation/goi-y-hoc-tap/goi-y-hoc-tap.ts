import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { Router, RouterLink } from '@angular/router';
import { RecommendationService } from '../../../services/recommendation.service';
import { ResponseObject } from '../../../responses/response-object';
import { RecommendationResponse } from '../../../responses/recommendation/recommendation-response';
import { CourseRecommendationItemResponse } from '../../../responses/recommendation/course-recommendation-item-response';
import { BoCauHoiRecommendationItemResponse } from '../../../responses/recommendation/bo-cau-hoi-recommendation-item-response';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-goi-y-hoc-tap',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './goi-y-hoc-tap.html',
  styleUrl: './goi-y-hoc-tap.scss',
})
export class GoiYHocTapComponent implements OnInit {
  loading = false;
  courses: CourseRecommendationItemResponse[] = [];
  boCauHoi: BoCauHoiRecommendationItemResponse[] = [];

  constructor(
    private recommendationService: RecommendationService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadRecommendations();
  }

  loadRecommendations() {
    this.loading = true;
    this.recommendationService.getMyRecommendations().subscribe({
      next: (res: ResponseObject<RecommendationResponse>) => {
        this.loading = false;
        this.courses = res.data?.courses ?? [];
        this.boCauHoi = res.data?.bo_cau_hoi ?? [];
      },
      error: (err) => {
        this.loading = false;
        const msg = err?.error?.message || 'Không thể tải gợi ý học tập';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  goToCourse(course: CourseRecommendationItemResponse) {
    const id = course.khoa_hoc.id;
    this.router.navigate(['/khoa-hoc', id]);
  }

  goToBoCauHoi(rec: BoCauHoiRecommendationItemResponse) {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', rec.bo_cau_hoi_id]);
  }
}


